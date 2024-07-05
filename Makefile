# Determines whether a DEBUG or RELEASE build is made. A value of 1 is a DEBUG
# build, while a value of 0 is a RELEASE build. To build in release mode
# run command make -B DEBUG=0.
DEBUG ?= 1
TARGET = luna
CFLAGS = -Wall -Werror -Wpedantic -ansi -MD -MP

# Setting DEBUG or release specific CFLAGS
ifeq ($(DEBUG), 1)
	CFLAGS += -g3 -O0 -fsanitize=address,undefined
else
	CFLAGS += -O2 -DNDEBUG
endif

.PHONY: all
all: $(TARGET)

SRCS := main.c lexer.c
OBJS := $(SRCS:.c=.o)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(TARGET) $(OBJS)

%.o: %.c
	$(CC) -c $(CFLAGS) $< -o $@

-include $(SRCS:.c=.d)

.PHONY: clean
clean:
	$(RM) -r $(TARGET) $(OBJS)
