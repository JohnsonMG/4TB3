from __future__ import print_function
from unicorn import *
from unicorn.arm64_const import *
import struct
import sys

code = sys.stdin.read()

# Processor preamble to enable floating point ops and NEON (SIMD) ops
# http://infocenter.arm.com/help/topic/com.arm.doc.den0024a/BABGBFBF.html
# http://infocenter.arm.com/help/topic/com.arm.doc.den0024a/CEGDJDJD.html
NEON_ENABLE = b"\x41\x10\x38\xd5"   # mrs x1, cpacr_el1
NEON_ENABLE += b"\x21\x04\x6c\xb2"  # orr x1, x1, #0x300000
NEON_ENABLE += b"\x41\x10\x18\xd5"  # msr cpacr_el1, x1
NEON_ENABLE += b"\xdf\x3f\x03\xd5"  # isb

# code to be emulated
ARM64_CODE = NEON_ENABLE + code

# memory address where emulation starts
ADDRESS = 0x1000000

# callback for tracing instructions
def hook_code(uc, address, size, user_data):
    print(">>> Tracing instruction at 0x%x, instruction size = 0x%x" %(address, size))

    reg_XN = []
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X9))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X10))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X11))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X12))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X13))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X14))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X15))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X16))

    j = 0
    for i in reg_XN:
        print(">>> X%d = 0x%x" %(j+9, i))
        j = j + 1

    print(">>> SP = 0x%x"%mu.reg_read(UC_ARM64_REG_SP))
    '''for i in range(0x108d010, 0x108d100, 4):
        tmp = mu.mem_read(i, 4)
        print(">>> Read 4 bytes from [0x%x] = 0x" %(i), end="")
        for i in reversed(tmp):
            print("%x" %(i), end="")
    print("")'''
    sys.stdout.flush()

def hook_intr(uc, exception_index, user_data):
    # TODO: Handle syscalls assuming they are the same as Linux.
    print("exception index: %d"%exception_index)

    # Handle SVC call
    if exception_index == 2:
        syscall_op = mu.reg_read(UC_ARM64_REG_X8)
        if syscall_op == 93:  # Exit syscall
            exit_code = mu.reg_read(UC_ARM64_REG_X0)
            print("Program exited with status %d"%exit_code)
            uc.emu_stop()
            return
        elif syscall_op == 63:  # Read syscall
            print("Requesting integer input of size %d"%mu.reg_read(UC_ARM64_REG_X2))
            fd = mu.reg_read(UC_ARM64_REG_X0)
            if fd != 0:
                print("Warning: Non stdin fd specified (%d)"%fd)
            addr = mu.reg_read(UC_ARM64_REG_X1)
            mu.mem_write(addr, struct.pack('<i', 40))
            return
        elif syscall_op == 64:  # Write syscall
            print("Requesting integer output of size %d"%mu.reg_read(UC_ARM64_REG_X2))
            fd = mu.reg_read(UC_ARM64_REG_X0)
            if fd != 1 and fd != 2:
                print("Warning: Non stdout/stderr fd specified (%d)"%fd)
            addr = mu.reg_read(UC_ARM64_REG_X1)
            #mu.mem_write(addr, struct.pack('<i', 40))
            sz = mu.reg_read(UC_ARM64_REG_X2)
            val = mu.mem_read(addr, sz)
            # Hack
            if sz == 1 and val == '\x0A':
                print(file=sys.stderr)
            else:
                print(struct.unpack('<i',str(val))[0], end='', file=sys.stderr)
            return
        else:
            print("CPU received unhandled software exception %d"%syscall_op)

    reg_XN = []
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X9))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X10))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X11))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X12))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X13))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X14))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X15))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X16))

    j = 0
    for i in reg_XN:
        print(">>> X%d = 0x%x" %(j+9, i))
        j = j + 1

    print(">>> W0 = 0x%x"%mu.reg_read(UC_ARM64_REG_W0))
    print(">>> W8 = 0x%x"%mu.reg_read(UC_ARM64_REG_W8))
    print(">>> W9 = 0x%x"%mu.reg_read(UC_ARM64_REG_W9))
    print(">>> W11 = 0x%x"%mu.reg_read(UC_ARM64_REG_W10))
    print(">>> W12 = 0x%x"%mu.reg_read(UC_ARM64_REG_W11))
    print(">>> W13 = 0x%x"%mu.reg_read(UC_ARM64_REG_W12))
    print(">>> W14 = 0x%x"%mu.reg_read(UC_ARM64_REG_W13))
    print(">>> W15 = 0x%x"%mu.reg_read(UC_ARM64_REG_W14))
    print(">>> W16 = 0x%x"%mu.reg_read(UC_ARM64_REG_W15))

    print(">>> V0 = 0x%x"%mu.reg_read(UC_ARM64_REG_V0))
    print(">>> V1 = 0x%x"%mu.reg_read(UC_ARM64_REG_V1))
    print(">>> V2 = 0x%x"%mu.reg_read(UC_ARM64_REG_V2))
    print(">>> V3 = 0x%x"%mu.reg_read(UC_ARM64_REG_V3))
    print(">>> V4 = 0x%x"%mu.reg_read(UC_ARM64_REG_V4))
    print(">>> V5 = 0x%x"%mu.reg_read(UC_ARM64_REG_V5))
    print(">>> V6 = 0x%x"%mu.reg_read(UC_ARM64_REG_V6))
    print(">>> V7 = 0x%x"%mu.reg_read(UC_ARM64_REG_V7))
    print(">>> V8 = 0x%x"%mu.reg_read(UC_ARM64_REG_V8))
    print(">>> V9 = 0x%x"%mu.reg_read(UC_ARM64_REG_V9))
    print(">>> V11 = 0x%x"%mu.reg_read(UC_ARM64_REG_V10))
    print(">>> V12 = 0x%x"%mu.reg_read(UC_ARM64_REG_V11))
    print(">>> V13 = 0x%x"%mu.reg_read(UC_ARM64_REG_V12))
    print(">>> V14 = 0x%x"%mu.reg_read(UC_ARM64_REG_V13))
    print(">>> V15 = 0x%x"%mu.reg_read(UC_ARM64_REG_V14))
    print(">>> V16 = 0x%x"%mu.reg_read(UC_ARM64_REG_V15))
    print(">>> V23 = 0x%x"%mu.reg_read(UC_ARM64_REG_V23))
    uc.emu_stop()

print("Emulate arm code")
try:
    # Initialize emulator in ARM-64bit mode
    mu = Uc(UC_ARCH_ARM64, UC_MODE_ARM)

    # map 2MB memory for this emulation
    mu.mem_map(ADDRESS, 10 * 1024 * 1024)

    # write machine code to be emulated to memory
    mu.mem_write(ADDRESS+0x658-len(NEON_ENABLE), ARM64_CODE)  # Offset is needed to keep the relative value of data the same

    # Shoehorn stack
    mu.reg_write(UC_ARM64_REG_SP, ADDRESS + 9 * 1024 * 1024)
    mu.reg_write(UC_ARM64_REG_X29, ADDRESS + 9 * 1024 * 1024)

    # tracing all instructions with customized callback
    mu.hook_add(UC_HOOK_CODE, hook_code)

    mu.hook_add(UC_HOOK_INTR, hook_intr)

    # initialize machine registers
    #mu.reg_write(UC_ARM64_REG_X11, 0x0)
    #mu.reg_write(UC_ARM64_REG_X13, 0x10)
    #mu.reg_write(UC_ARM64_REG_X15, 0x3)
    # mu.reg_write(UC_ARM64_REG_X0, 0x7890)
    # etc...

    # emulate code in infinite time & unlimited instructions
    print(len(ARM64_CODE))
    mu.emu_start(ADDRESS+0x658-len(NEON_ENABLE), len(ARM64_CODE))

    # now print out some registers
    print("Emulation done. Below is the CPU context")

    reg_XN = []
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X9))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X10))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X11))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X12))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X13))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X14))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X15))
    reg_XN.append(mu.reg_read(UC_ARM64_REG_X16))

    j = 0
    for i in reg_XN:
        print(">>> X%d = 0x%x" %(j+9, i))
        j = j + 1

    print(">>> W9 = 0x%x"%mu.reg_read(UC_ARM64_REG_W9))
    print(">>> W11 = 0x%x"%mu.reg_read(UC_ARM64_REG_W10))
    print(">>> W12 = 0x%x"%mu.reg_read(UC_ARM64_REG_W11))
    print(">>> W13 = 0x%x"%mu.reg_read(UC_ARM64_REG_W12))
    print(">>> W14 = 0x%x"%mu.reg_read(UC_ARM64_REG_W13))
    print(">>> W15 = 0x%x"%mu.reg_read(UC_ARM64_REG_W14))
    print(">>> W16 = 0x%x"%mu.reg_read(UC_ARM64_REG_W15))

    print(">>> V0 = 0x%x"%mu.reg_read(UC_ARM64_REG_V0))
    print(">>> V1 = 0x%x"%mu.reg_read(UC_ARM64_REG_V1))
    print(">>> V2 = 0x%x"%mu.reg_read(UC_ARM64_REG_V2))
    print(">>> V3 = 0x%x"%mu.reg_read(UC_ARM64_REG_V3))
    print(">>> V4 = 0x%x"%mu.reg_read(UC_ARM64_REG_V4))
    print(">>> V5 = 0x%x"%mu.reg_read(UC_ARM64_REG_V5))
    print(">>> V6 = 0x%x"%mu.reg_read(UC_ARM64_REG_V6))
    print(">>> V7 = 0x%x"%mu.reg_read(UC_ARM64_REG_V7))
    print(">>> V8 = 0x%x"%mu.reg_read(UC_ARM64_REG_V8))
    print(">>> V9 = 0x%x"%mu.reg_read(UC_ARM64_REG_V9))
    print(">>> V11 = 0x%x"%mu.reg_read(UC_ARM64_REG_V10))
    print(">>> V12 = 0x%x"%mu.reg_read(UC_ARM64_REG_V11))
    print(">>> V13 = 0x%x"%mu.reg_read(UC_ARM64_REG_V12))
    print(">>> V14 = 0x%x"%mu.reg_read(UC_ARM64_REG_V13))
    print(">>> V15 = 0x%x"%mu.reg_read(UC_ARM64_REG_V14))
    print(">>> V16 = 0x%x"%mu.reg_read(UC_ARM64_REG_V15))
    print(">>> V21 = 0x%x"%mu.reg_read(UC_ARM64_REG_V21))
    print(">>> V23 = 0x%x"%mu.reg_read(UC_ARM64_REG_V23))
    print(">>> V24 = 0x%x"%mu.reg_read(UC_ARM64_REG_V24))
    print(">>> V25 = 0x%x"%mu.reg_read(UC_ARM64_REG_V25))
    print(">>> V29 = 0x%x"%mu.reg_read(UC_ARM64_REG_V29))

    for i in range(0x108e000, 0x108e160, 4):
        tmp = mu.mem_read(i, 4)
        print(">>> Read 4 bytes from [0x%x] = 0x" %(i), end="")
        for i in reversed(tmp):
            print("%x" %(i), end="")
        print("")
 
except Exception as e:
    print("ERROR: %s" % e) 