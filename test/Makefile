all:
	gcc -o example example.c
	arm-linux-gnueabi-gcc -std=c99 -g -o fib-arm fib.c
	gcc -std=c99 -g -o fib-x64 fib.c
	gcc -std=c99 -g -m32 -o fib-x86 fib.c
