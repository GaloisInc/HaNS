
OBJS = tapdevice.o

ifndef V
	QUIET_CC = @echo '   ' CC $@;
endif

all : $(OBJS) send receive

send : send.c
	$(QUIET_CC)$(CC) -Wall -o $@ $<

receive : receive.c
	$(QUIET_CC)$(CC) -Wall -o $@ $<

tapdevice.o : tapdevice.c
	$(QUIET_CC)$(CC) -Wall -c -o $@ $<

clean:
	$(RM) tapdevice.o
	$(RM) send
	$(RM) receive
