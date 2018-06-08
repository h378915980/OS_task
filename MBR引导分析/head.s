/*
* linux/boot/head.s
*
* (C) 1991 Linus Torvalds
*/

/*
* head.s contains the 32-bit startup code.
*
* NOTE!!! Startup happens at absolute address 0x00000000, which is also where
* the page directory will exist. The startup code will be overwritten by
* the page directory.
*/
/*
* head.s 含有32 位启动代码。
* 注意 32 位启动代码是从绝对地址0x00000000 开始的，这里也同样是页目录将存在的地方，
* 因此这里的启动代码将被页目录覆盖掉。
*/
.text
.globl _idt,_gdt,_pg_dir,_tmp_floppy_area
_pg_dir: 				/* 页目录将会存放在这里。	*/



startup_32: 			/* 18-22 行设置各个数据段寄存器。	*/
	movl $0x10,%eax 
				
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	mov %ax,%gs

lss _stack_start,%esp 		/* 设置系统堆栈。	*/
			
	call setup_idt 				/* 调用设置中断描述符表子程序。	*/
	call setup_gdt 				/* 调用设置全局描述符表子程序。	*/
	movl $0x10,%eax 			/* reload all the segment registers	*/
	mov %ax,%ds 				/* after changing gdt. CS was already	*/
	mov %ax,%es 				/* reloaded in 'setup_gdt'
	mov %ax,%fs 				/* 因为修改了gdt，所以需要重新装载所有的段寄存器。	*/
	mov %ax,%gs 				/* CS 代码段寄存器已经在setup_gdt 中重新加载过了。	*/
	lss _stack_start,%esp
				/* 用于测试A20 地址线是否已经开启。采用的方法是向内存地址0x000000 处写入任意*/
				/* 一个数值，然后看内存地址0x100000(1M)处是否也是这个数值。如果一直相同的话，就一直	*/
				/* 比较下去，也即死循环、死机。表示地址A20 线没有选通，结果内核就不能使用1M 以上内存。*/
	xorl %eax,%eax
1: incl %eax 					/* check that A20 really IS enabled	*/
	movl %eax,0x000000 			/* loop forever if it isn't	*/
	cmpl %eax,0x100000
	je 1b 						
/*
* NOTE! 486 should set bit 16, to check for write-protect in supervisor
* mode. Then it would be unnecessary with the "verify_area()"-calls.
* 486 users probably want to set the NE (				/*5) bit also, so as to use
* int 16 for math errors.
*/

				
				
	movl %cr0,%eax 				/* check math chip	*/
	andl $0x80000011,%eax 		/* Save PG,PE,ET	*/
/* "orl $0x10020,%eax" here for 486 might be good */
	orl $2,%eax 				/* set MP	*/
	movl %eax,%cr0
	call check_x87
	jmp after_page_tables 		

/*
* We depend on ET to be correct. This checks for 287/387.
*/

check_x87:
	fninit
	fstsw %ax
	cmpb $0,%al
	je 1f 						/* no coprocessor: have to set bits */
	movl %cr0,%eax 				/* 如果存在的则向前跳转到标号1处，否则改写cr0。*/
	xorl $6,%eax 				/* reset MP, set EM */
	movl %eax,%cr0
	ret
.align 2 						
								/* 按4 字节方式对齐内存地址。*/
	1: .byte 0xDB,0xE4 			/* fsetpm for 287, ignored by 387  287 协处理器码。*/
	ret

/*
* setup_idt
*
* sets up a idt with 256 entries pointing to
* ignore_int, interrupt gates. It then loads
* idt. Everything that wants to install itself
* in the idt-table may do so themselves. Interrupts
* are enabled elsewhere, when we can be relatively
* sure everything is ok. This routine will be over-
* written by the page tables.
*/
/*
* 下面这段是设置中断描述符表子程序 setup_idt
*
* 将中断描述符表idt 设置成具有256 个项，并都指向ignore_int中断门。然后加载中断
* 描述符表寄存器(用lidt指令)。真正实用的中断门以后再安装。当其它地方一切
* 都正常时再开启中断。该子程序将会被页表覆盖掉。
*/
/*中断描述符表中的项虽然也是8字节组成，但其格式与全局表中的不同，被称为门描述符
*(Gate Descriptor)。它的0-1，6-7字节是偏移量，2-3字节是选择符，4-5字节是一些标志。 
*这段代码首先在edx、eax中组合设置出8字节默认的中断描述符值，然后在idt表每一项中 
*都放置该描述符，共256项。eax含有描述符低4字节，edx含有高4字节。内核在随后的初始 
*化过程中会替换安装那些真正实用的中断描述符项。
*/
setup_idt:
	lea ignore_int,%edx 				/* 将ignore_int的有效地址值——>edx 寄存器	*/
	movl $0x00080000,%eax 				/* 将选择符0x0008 置入eax 的高16 位中。	*/
	movw %dx,%ax /* selector = 0x0008 = cs */
				/* 偏移值的低16 位置入eax 的低16 位中。此时eax 含有	*/
				/*门描述符低4 字节的值。	*/
	movw $0x8E00,%dx /* interrupt gate - dpl=0, present */
				/* 此时edx 含有门描述符高4 字节的值。	*/
	lea _idt,%edi 						/* _idt是中断描述符表的地址。	*/
	mov $256,%ecx
rp_sidt:
	movl %eax,(%edi) 					/* 将哑中断门描述符存入表中。	*/
	movl %edx,4(%edi)
	addl $8,%edi 						/* edi指向表中下一项。	*/
	dec %ecx
	jne rp_sidt
	lidt idt_descr 						/* 加载中断描述符表寄存器值。	*/
	ret

/*
* setup_gdt
*
* This routines sets up a new gdt and loads it.
* Only two entries are currently built, the same
* ones that were built in init.s. The routine
* is VERY complicated at two whole lines, so this
* rather long comment is certainly needed :-).
* This routine will beoverwritten by the page tables.
*/
/*设置全局描述符表项setup—gdt
*这个子程序设置一个新的全局描述符表gdt，并加载。此时仅创建了两个表项，与前 
*面的一样。
*/

setup_gdt:
	lgdt gdt_descr 				/* 加载全局描述符表寄存器。	*/
	ret

/*
* I put the kernel page tables right after the page directory,
* using 4 of them to span 16 Mb of physical memory. People with
* more than 16MB will have to expand this.
*/

/* 每个页表长为4 Kb 字节，而每个页表项需要4 个字节，因此一个页表共可以存放1000 个表项，
* 如果一个表项寻址4 Kb 的地址空间，则一个页表就可以寻址4 Mb 的物理内存。
* 页表项的格式为：项的前0-11 位存放一些标志，如是否在内存中(P 位0)、读写许可(R/W 位1)、
* 普通用户还是超级用户使用(U/S 位2)、是否修改过(D 位6)等；表项的位12-31 是
* 页框地址，用于指出一页内存的物理起始地址。
*/
.org 0x1000 	/* 从偏移0x1000 处开始是第1 个页表（偏移0 开始处将存放页表目录）。	*/
pg0:

.org 0x2000
pg1:

.org 0x3000
pg2:

.org 0x4000
pg3:

.org 0x5000 	/* 定义下面的内存数据块从偏移0x5000 处开始。	*/
/*
* tmp_floppy_area is used by the floppy-driver when DMA cannot
* reach to a buffer-block. It needs to be aligned, so that it isn't
* on a 64kB border.
*/
/* 当DMA（直接存储器访问）不能访问缓冲块时，下面的tmp_floppy_area 内存块
* 就可供软盘驱动程序使用。其地址需要对齐调整，这样就不会跨越64kB 边界。
*/
_tmp_floppy_area:
	.fill 1024,1,0 				/* 共保留1024 项，每项1 字节，填充数值0。	*/
/*
# 下面这几个入栈操作用于为跳转到init/main.c中的main()函数作准备工作。
#指令在栈中压入了返回地址，而第140行则压入了mainO函数代码的地址。当head.s 
# 最后在执行ret指令时就会弹出main()的地址，并把控制权转移到init/main.c 
# 程序中。
# 前面3个入栈0值应该分别表示envp、argv指针和argc的值，但main()没有用到。
# 139行的入栈操作是模拟调用main.c程序时首先将返回地址入栈的操作，所以如果 
# main.C程序真的退出时，就会返回到这里的标号L6处继续执行下去，也即死循环。
# 将main.c的地址压入堆栈，这样，在设置分页处理（setup—paging）结束后 
# 执行’ret’返回指令时就会将main.c程序的地址弹出堆栈，并去执行main.c程序了。
*/
after_page_tables:
	pushl $0 					/* These are the parameters to main :-)	*/
	pushl $0 					/* 这些是调用main 程序的参数。	*/
	pushl $0
	pushl $L6 					/* return address for main, if it decides to.	*/
	pushl $_main 				/* '_main'是编译程序对main 的内部表示方法。	*/
	jmp setup_paging 		
L6:
	jmp L6 						/* main should never return here, but	*/
				/* just in case, we know what happens.	*/

/* This is the default interrupt "handler" :-) */
/* 下面是默认的中断“向量句柄”? */
int_msg:
	.asciz "Unknown interrupt\n\r" 		/* 定义字符串“未知中断(回车换行)”。*/
.align 2 						/* 按4 字节方式对齐内存地址。	*/
ignore_int:
	pushl %eax
	pushl %ecx
	pushl %edx
	push %ds 					/*ds,es,fs,gs 等虽然是16 位的寄存器，但入栈后	*/
								/* 仍然会以32 位的形式入栈，也即需要占用4 个字节的堆栈空间。*/
	push %es
	push %fs
	movl $0x10,%eax 			/* 置段选择符。	*/
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	pushl $int_msg 				/* 把调用printk函数的参数指针（地址）入栈。注意！若符号int_msg*/
								/* 前不加'$'，则表示把int_msg符号处的长字（’ Unkn’）入栈?。*/
	call _printk 				/* 该函数在/kernel/printk.c 中。*/
								/* '_printk'是printk 编译后模块中的内部表示法。*/
	popl %eax
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret 						/* 中断返回（把中断调用时压入栈的CPU 标志寄存器（32 位）值也弹出）。*/


/*
* Setup_paging
*
* This routine sets up paging by setting the page bit
* in cr0. The page tables are set up, identity-mapping
* the first 16MB. The pager assumes that no illegal
* addresses are produced (ie >4Mb on a 4Mb machine).
*
* NOTE! Although all physical memory should be identity
* mapped by this routine, only the kernel page functions
* use the >1Mb addresses directly. All "normal" functions
* use just the lower 1Mb, or the local data space, which
* will be mapped to some other place - mm keeps track of
* that.
*
* For those with more memory than 16 Mb - tough luck. I've
* not got it, why should you :-) The source is here. Change
* it. (Seriously - it shouldn't be too difficult. Mostly
* change some constants etc. I left it at 16Mb, as my machine
* even cannot be extended past that (ok, but it was cheap :-)
* I've tried to show which constants to change by having
* some kind of marker at them (search for "16Mb"), but I
* won't guarantee that's all :-( )
*/


.align 2 						/* 按4 字节方式对齐内存地址边界。*/
setup_paging:					/* 首先对5 页内存（1 页目录 + 4 页页表）清零*/
	movl $1024*5,%ecx /* 5 pages - pg_dir+4 page tables */
	xorl %eax,%eax
	xorl %edi,%edi /* pg_dir is at 0x000 */
				/* 页目录从0x000 地址开始。	*/
	cld;rep;stosl
				 * 页目录项的结构与页表中项的结构一样，4 个字节为1 项。参见上面113 行下的说明。
				 * "$pg0+7"表示：0x00001007，是页目录表中的第1 项。
				 * 则第1 个页表所在的地址 = 0x00001007 & 0xfffff000 = 0x1000；
				 * 第1 个页表的属性标志 = 0x00001007 & 0x00000fff = 0x07，表示该页存在、用户可读写。
				 */
	movl $pg0+7,_pg_dir /* set present bit/user r/w */
	movl $pg1+7,_pg_dir+4 /* --------- " " --------- */
	movl $pg2+7,_pg_dir+8 /* --------- " " --------- */
	movl $pg3+7,_pg_dir+12 /* --------- " " --------- */
				/* 填写4 个页表中所有项的内容，共有：4(页表)*1024(项/页表)=4096 项(0 - 0xfff)，
				 * 也即能映射物理内存 4096*4Kb = 16Mb。
				 * 每项的内容是：当前项所映射的物理内存地址 + 该页的标志（这里均为7）。
				 * 使用的方法是从最后一个页表的最后一项开始按倒退顺序填写。一个页表的最后一项在页表中的
				 * 位置是1023*4 = 4092。因此最后一页的最后一项的位置就是$pg3+4092。
				 */
	movl $pg3+4092,%edi 			/* edi→最后一页的最后一项。	*/
	movl $0xfff007,%eax /* 16Mb - 4096 + 7 (r/w user,p) */
				/* 最后1 项对应物理内存页面的地址是0xfff000，加上属性标志7，即为0xfff007.	*/
	std 		/* 方向位置位，edi 值递减(4 字节)。	*/
1: stosl 		/* fill pages backwards - more efficient :-) */
	subl $0x1000,%eax 				/* 每填写好一项，物理地址值减0x1000。	*/
	jge 1b 		/* 如果小于0 则说明全添写好了。设置页目录基址寄存器cr3 的值，指向页目录表。	*/
	xorl %eax,%eax /* pg_dir is at 0x0000  页目录表在0x0000 处。	*/
	movl %eax,%cr3 /* cr3 - page directory start */
				/* 设置启动使用分页处理（cr0 的PG 标志，位31）	*/
	movl %cr0,%eax
	orl $0x80000000,%eax 			/* 添上PG 标志。	*/
	movl %eax,%cr0 					/* set paging (PG) bit */
	ret 		/* this also flushes prefetch-queue */
				/* 在改变分页处理标志后要求使用转移指令刷新预取指令队列，这里用的是返回指令ret。	*/
				/* 该返回指令的另一个作用是将堆栈中的main 程序的地址弹出，并开始运行/init/main.c 程序。	*/
				/* 本程序到此真正结束了。	*/

.align 2 							/* 按4 字节方式对齐内存地址边界。	*/
.word 0
/* 下面是加载中断描述符表寄存器idtr的指令lidt要求的6字节操作数。前2字节是idt表的限长，后4字节是idt表在线性地址空间中的32位基地址。*/
idt_descr: 							/*下面两行是lidt 指令的6 字节操作数：长度，基址。	*/
	.word 256*8-1 					/* idt contains 256 entries	*/
	.long _idt
.align 2
.word 0
/* 下面加载全局描述符表寄存器gdtr的指令lgdt要求的6字节操作数。前2字节是gdt表的限长，
*后4字节是gdt表的线性基地址。这里全局表长度设置为2KB字节（0x7ff即可），因为每8字节 
*组成一个描述符项，所以表中共可有256项。符号—gdt是全局表在本程序中的偏移位置，见234行
*/
gdt_descr: 							/* 下面两行是lgdt 指令的6 字节操作数：长度，基址。	*/
	.word 256*8-1 					/* so does gdt (not that that's any	*/
	.long _gdt 						/* magic number, but it works for me :^)	*/

.align 3 							/* 按8 字节方式对齐内存地址边界。	*/
_idt: .fill 256,8,0 				/* idt is uninitialized 256 项，每项8 字节，填0。	*/
/*
#全局表。前4项分别是空项、代码段描述符、数据段描述符、系统调用段描述符
#(0-nul, 1-cs，2-ds, 3-syscall, 4-TSS0，5-LDTO, 6-TSS1，7-LDT1，8-TSS2 etc...)
*/
  _gdt: .quad 0x0000000000000000 		/* NULL descriptor */
	.quad 0x00c09a0000000fff 		/* 16Mb 	 代码段最大长度16M。	*/
	.quad 0x00c0920000000fff 		/* 16Mb 	 数据段最大长度16M。	*/
	.quad 0x0000000000000000 		/* TEMPORARY - don't use */
	.fill 252,8,0 					/* space for LDT's and TSS's etc */
