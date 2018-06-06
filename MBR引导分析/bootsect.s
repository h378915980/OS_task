!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux 
!
! SYS_SIZE是要加载的系统模块长度，单位是节，16字节为1节。0x3000共为196 kB，对于当前的版本空间已足够了。 

	
SYSSIZE = 0x3000 
! system的大小。

!
! bootsect.s (C) 1991 Linus Torvalds
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps there.
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts.
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.
!
! bootsect.s被bios启动子程序加载至0x7c00处，并将自己移到了地址0x90000处，并跳转至那里。
!
! 它然后使用BIOS中断将'setup'直接加载到自己的后面(0x90200),并将system加载到地址0x10000处。

.globl begtext, begdata, begbss, endtext, enddata, endbss ! 定义了6 个全局标识符；
.text 							! 文本段；
begtext:
.data							! 数据段；
begdata:
.bss 							! 堆栈段；
begbss:
.text							! 文本段；
SETUPLEN = 4					! nr of setup-sectors	
								! setup 程序的扇区数(setup-sectors)值；
BOOTSEG  = 0x07c0 				! original address of boot-sector
								! bootsect 的原始地址；
INITSEG  = 0x9000 				! we move boot here - out of the way
								! 将bootsect 移到这里；
SETUPSEG = 0x9020 				! setup starts here
								! setup 程序从这里开始；
SYSSEG   = 0x1000 				! system loaded at 0x10000 (65536).
								! system 模块加载到0x10000处；
ENDSEG   = SYSSEG + SYSSIZE 	! where to stop loading
								! 停止加载的段地址；
! ROOT_DEV: 0x000 - same type of floppy as boot.
								
ROOT_DEV = 0x306 
! 设备号0x306指定根文件系统设备是第2个硬盘的第1个分区。

entry start						! 程序从start标号开始执行。
start: 							! 将自身(bootsect)从目前段位置0x07c0移动到0x9000(576k)处，共256 字,然后跳转到 移动后代码的go 标号处,也即本程序的下一语句处。
	mov ax,#BOOTSEG 			! 将ds 段寄存器置为0x7C0；
	mov ds,ax
	mov ax,#INITSEG 			
	mov es,ax					! 将es 段寄存器置为0x9000；	
	mov cx,#256 				! 移动计数值=256 字；
	sub si,si 					! 源地址		ds:si = 0x07C0:0x0000
	sub di,di 					! 目的地址	es:di = 0x9000:0x0000
	rep 						! 重复执行，直到cx = 0
	movw 						! 移动1 个字；
	jmpi go,INITSEG 			! 间接跳转,INITSEG说明跳转到的段地址。

! 此时CPU已移动到0x90000执行。

go: mov ax,cs 					! 将ds、es 和ss 都置成移动后代码所在的段处(0x9000)。
	mov ds,ax
	mov es,ax
! put stack at 0x9ff00. 		! 将堆栈指针sp 指向0x9ff00处
	mov ss,ax					! 设置堆栈。
	mov sp,#0xFF00 				! arbitrary value >>512



! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.
! 在bootsect 程序块后紧根着加载setup 模块的代码数据。
! 注意es 已经设置好了(es已经指向目的段地址处0x9000)。

load_setup:						! 利用BIOS中断INT0x13将setup模块从磁盘第2个扇区开始读到0x90200 开始处，共读4个扇区。
								! 如果读出错，则复位驱动器，并重试。
								
	mov dx,#0x0000				! drive 0, head 0
	mov cx,#0x0002				! sector 2, track 0
	mov bx,#0x0200				! address = 512, in INITSEG
	mov ax,#0x0200+SETUPLEN		! service 2, nr of sectors
	int 0x13					! read it
	jnc ok_load_setup			! ok - continue
	mov dx,#0x0000				!对驱动器 0 进行读操作。
	mov ax,#0x0000				! reset the diskette
	int 0x13
	j	load_setup				! 即 MPS 指令。
ok_load_setup:
! Get disk drive parameters, specifically nr of sectors/track
								! 取磁盘驱动器的参数，特别是每道的扇区数量。
								! 取磁盘驱动器参数INT 0x13 调用格式和返回信息如下：
								! ah = 0x08 dl = 驱动器号（如果是硬盘则要置位7 为1）。
								! 返回信息：
								! ch = 最大磁道号的低8 位，cl = 每磁道最大扇区数，最大磁道号高2位
								
	mov dl,#0x00
	mov ax,#0x0800 				! AH=8 is get drive parameters
	int 0x13
	mov ch,#0x00
	seg cs 						! 表示下一条语句的操作数在cs 段寄存器所指的段中。
								!下句保存每磁道扇区数。
	mov sectors,cx 				! 保存每磁道扇区数。
	mov ax,#INITSEG
	mov es,ax 					! 因为上面取磁盘参数中断改掉了es 的值，这里重新改回。

! Print some inane message
! 显示信息

! BIOS中断0x10功能号ah = 0x03，读光标位置。
! 输入：bh =页号
! 返回：ch =扫描开始线；cl =扫描结束线；dh =行号(0x00顶端)；dl =列号(0x00最左边)。
! BIOS中断0x10功能号ah = 0x13，显示字符串。
! 输入：al =放置光标的方式及规定属性。0x01-表示使用bl中的属性值，光标停在字符串结尾处。
!es:bp此寄存器对指向要显示的字符串起始位置处。cx =显示的字符串字符数。bh =显示页面号；
! bl =字符属性。dh =行号；dl =列号。

	mov ah,#0x03				! read cursor pos
	xor bh,bh					! 读光标位置。
	int 0x10
	mov cx,#24 					! 共24 个字符。
	mov bx,#0x0007 				! page 0, attribute 7 (normal)
	mov bp,#msg1 				! 指向要显示的字符串。
	mov ax,#0x1301 				! write string, move cursor
	int 0x10 					! 写字符串并移动光标。

! ok, we’ve written the message, now we want to load the system (at 0x10000) 
! 现在开始将system 模块加载到0x10000处。

	mov ax,#SYSSEG
	mov es,ax					! segment of 0x010000 ! es = 存放system 的段地址。
	call read_it				! 读磁盘上system 模块，es 为输入参数。
	call kill_motor 			! 关闭驱动器马达，这样就可以知道驱动器的状态了。

!  After that we check which root-device to use. If the device is
!  defined (= 0), nothing is done and the given device is used.
!  Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
!  on the number of sectors that the BIOS reports currently.

! 此后，我们检查要使用哪个根文件系统设备（简称根设备）。如果已经指定了设备（=0）
! 就直接使用给定的设备。否则就需要根据BIOS报告的每磁道扇区数来 
! 确定到底使用/dev/PSO (2,28)还是 /dev/atO (2，8)。


	seg cs
	mov ax,root_dev 	! 将根设备号
	cmp ax,#0
	jne root_defined
	seg cs
	mov bx,sectors 			
	mov ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp bx,#15 			! 判断每磁道扇区数是否=15
	je root_defined 	! 如果等于，则ax 中就是引导驱动器的设备号。
	mov ax,#0x021c 		! /dev/PS0 - 1.44Mb
	cmp bx,#18
	je root_defined
undef_root:				! 如果都不一样，则死循环（死机）。
	jmp undef_root
root_defined:
	seg cs
	mov root_dev,ax ! 将检查过的设备号保存起来。

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:
! 到此，所有程序都加载完毕，我们就跳转到被加载在bootsect后面的setup程序去。
! 段间跳转指令（Jump Intersegment）。跳转到0x9020:0000(setup.s程序开始处)去执行。

	jmpi 0,SETUPSEG 	! 跳转到0x9020:0000(setup.s 程序的开始处)。
						! 本程序到此就结束了。
						
! 下面是两个子程序。read—it用于读取磁盘上的system模块。kill—moter用于关闭软驱的马达。

! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in: es - starting address segment (normally 0x1000)
!
! 该子程序将系统模块加载到内存地址0x10000处，并确定没有跨越64KB的内存边界。
! 我们试图尽快地进行加载，只要可能，就每次加载整条磁道的数据。
! 输入：es -开始内存地址段值（通常是0x1000） 
! 下面伪操作符.word定义一个2字节目标。相当于C语言程序中定义的变量和所占内存空间大小。
! ’1+SETUPLEN’表示开始时已经读进1个引导扇区和setup程序所占的扇区数SETUPLEN。

sread: .word 1+SETUPLEN ! sectors read of current track
						! 当前磁道中已读的扇区数。
						! 开始时已经读进1扇区的引导扇区
						! bootsect 和setup 程序所占的扇区数SETUPLEN。
head: .word 0			! current head !当前磁头号。
track: .word 0			! current track !当前磁道号。

read_it:

! 首先测试输入的段值。从盘上读入的数据必须存放在位于内存地址64KB的边界开始处，否则进入死 
! 循环。清bx寄存器，用于表示当前段内存放数据的开始位置。
! 153行上的指令test以比特位逻辑与两个操作数。若两个操作数对应的比特位都为1，则结果值的
! 对应比特位为1，否则为0。该操作结果只影响标志（零标志ZF等）。例如，若AX=0xl000，那么
! test指令的执行结果是(0x1000 & 0x0fff) = 0x0000，于是ZF标志置位。此时即下一条指令jne
! 条件不成立。

	mov ax,es
	test ax,#0x0fff
die: jne die 			! es must be at 64kB boundary ! es 值必须位于64KB 地址边界!
	xor bx,bx 			! bx is starting address within segment	! bx 为段内偏移位置。
rp_read:

! 接着判断是否已经读全入部数据。比较当前所读段是否就是系统数据末端所处的段(#ENDSEG)，如果
! 不是就跳转至下面 ok1_read 标号处继续读数据。否则退出子程序返回。

	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet? ! 
	jb ok1_read
	ret
ok1_read:
						! 计算和验证当前磁道需要读取的扇区数，放在ax 寄存器中。
						! 根据当前磁道还未读取的扇区数以及段内数据字节开始偏移位置，计算如果全部读取这些未读扇区，所
						! 读总字节数是否会超过64KB 段长度的限制。若会超过，则根据此次最多能读入的字节数(64KB – 段内
						! 偏移位置)，反算出此次需要读取的扇区数。
	seg cs
	mov ax,sectors		! 取每磁道扇区数。
	sub ax,sread 		! 减去当前磁道已读扇区数。
	mov cx,ax 			! cx = ax = 当前磁道未读扇区数。
	shl cx,#9 			! cx = cx * 512 字节。
	add cx,bx 			! cx = cx + 段内当前偏移值(bx)
						!    = 此次读操作后，段内共读入的字节数。
	jnc ok2_read 		! 若没有超过64KB 字节，则跳转至ok2_read 处执行。
	je ok2_read
! 若加上此次将读磁道上所有未读扇区时会超过64KB，则计算此时最多能读入的字节数：
! (b4.%?段内读偏移位置)，再转换成需读取的扇区数。其中 0 减某数就是取该数64KB 的补值。

	xor ax,ax 			! 若加上此次将读磁道上所有未读扇区时会超过64KB，则计算
	sub ax,bx 			! 此时最多能读入的字节数(64KB – 段内读偏移位置)，再转换
	shr ax,#9 			! 成需要读取的扇区数。
ok2_read:
! 读当前磁道上指定开始扇区（cl）和需读扇区数（al）的数据到 es:bx 开始处。然后统计当前磁道
! 上已经读取的扇区数并且磁道最大扇区数 sectors 作比较。如果小于 sectors 说明当前磁道上的还
! 有扇区未读。于是跳转到 RN3BUHDG 处继续操作。
	call read_track
	mov cx,ax 			! cx = 该次操作已读取的扇区数。
	add ax,sread 		! 当前磁道上已经读取的扇区数。
	seg cs
	cmp ax,sectors 		! 如果当前磁道上的还有扇区未读，则跳转到ok3_read 处。
	jne ok3_read
						! 读该磁道的下一磁头面(1 号磁头)上的数据。
						! 如果已经完成，则去读下一磁道。
	mov ax,#1
	sub ax,head 		! 判断当前磁头号。
	jne ok4_read 		! 如果是0 磁头，则再去读1 磁头面上的扇区数据。
	inc track 			! 否则去读下一磁道。
ok4_read:
	mov head,ax 		! 保存当前磁头号。
	xor ax,ax 			! 清当前磁道已读扇区数。
ok3_read:
! 如果当前磁道上的还有未读扇区，则首保先存当前磁道已读扇区数，然后调整存放数据处的开始
! 位置。若小于64KB 边界值，则跳转到 rp_read(156 行)处，继续读数据。
	mov sread,ax 		! 保存当前磁道已读扇区数。
	shl cx,#9 			! 上次已读扇区数*512 字节。
	add bx,cx 			! 调整当前段内数据开始位置。
	jnc rp_read 		! 若小于64KB 边界值，则跳转到rp_read(156 行)处，继续读数据。
						! 否则说明已经读取64KB 数据。此时调整当前段，为读下一段数据作准备。
	mov ax,es
	add ax,#0x1000 		! 将段基址调整为指向下一个64KB 段内存。
	mov es,ax
	xor bx,bx 			! 清段内数据开始偏移值。
	jmp rp_read 		! 跳转至rp_read(156 行)处，继续读数据。

! read_track 子程序。
! 读当前磁道上指定开始扇区和需读扇区数的数据到es:bx 开始处。
! 参见第67 行下对BIOS 磁盘读中断
! int 0x13，ah=2 的说明。
! al 		– 需读扇区数；
! es:bx 	– 缓冲区开始位置。
read_track:
	push ax
	push bx
	push cx
	push dx
	mov dx,track 		! 取当前磁道号。
	mov cx,sread 		! 取当前磁道上已读扇区数。
	inc cx 				! cl = 开始读扇区。
	mov ch,dl 			! ch = 当前磁道号。
	mov dx,head 		! 取当前磁头号。
	mov dh,dl 			! dh = 磁头号。
	mov dl,#0 			! dl = 驱动器号(为0 表示当前驱动器)。
	and dx,#0x0100 		! 磁头号不大于1。
	mov ah,#2 			! ah = 2，读磁盘扇区功能号。
	int 0x13
	jc bad_rt 			! 若出错，则跳转至bad_rt。
	pop dx
	pop cx
	pop bx
	pop ax
	ret

!  读磁盘操作出错。则执行驱动器复位操作（磁盘中断功能号 0），再跳转到 read_track 处重试。
bad_rt: mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track
/*
* This procedure turns off the floppy drive motor, so
* that we enter the kernel in a known state, and
* don't have to worry about it later.
*/
! 这个子程序用于关闭软驱的马达，这样我们进入内核后它处于已知状态，以后也就无须担心它了。

! 下面第 235 行上的值 0x3I2 是软盘控制器的一个端口，被称为数字输出寄存器（DOR）端口。它是
! 一个 8 位的寄存器，其位 7––位 4 分别用于控制 4 个软驱（D––A）的启动和关闭。位 3––位 2 用于
! 允许/禁止 DMA 和中断请求以及启动/复位软盘控制器 FDC。 位 1––位 0 用于选择选择操作的软驱。
! 第 236 行上在 alDO 中设置并输出的 0 值，就是用于选择 A 驱动器，关闭 FDC，禁止 DMA 和中断请求，
! 关闭马达。有关软驱控制卡编程的详细信息请参见  kernel/blk_drv/floppy.c 程序后面的说明。 
kill_motor:
	push dx
	mov dx,#0x3f2 			! 软驱控制卡的驱动端口，只写。
	mov al,#0 				! A 驱动器，关闭FDC，禁止DMA 和中断请求，关闭马达。
	outb 					! 将al 中的内容输出到dx 指定的端口去。
	pop dx
	ret
sectors:
	.word 0 				! 存放当前启动软盘每磁道的扇区数。
msg1:						! 调用 BIOS 中断显示的信息。
	.byte 13,10 			! 回车、换行的ASCII 码。
	.ascii "Loading system ..."
	.byte 13,10,13,10 		! 共24 个ASCII 码字符。
	
! 表示下面语句从地址 508(0x1FC)开始，所以 root_dev 在启动扇区的第 508 开始的 2 个字节中。
.org 508 					! 表示下面语句从地址508(0x1FC)开始，所以root_dev
							! 在启动扇区的第508 开始的2 个字节中。
root_dev:
	.word ROOT_DEV 			! 这里存放根文件系统所在的设备号
!  下面是启动盘具有有效引导扇区的标志。仅供 BIOS 中的程序加载引导扇区时识别使用。它必须位于
! 引导扇区的最后两个字节中。
boot_flag:
	.word 0xAA55 			! 硬盘有效标识。
.text
endtext:
.data
enddata:
.bss
endbss:
