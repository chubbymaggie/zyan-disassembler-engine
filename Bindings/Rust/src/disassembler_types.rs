/**
 * Verteron Disassembler Engine Rust language bindings
 * 
 * The MIT License (MIT)
 * 
 * Copyright (c) 2015 athre0z
 * Copyright (c) 2015 Florian Bernd
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#[allow(dead_code)] // TODO: remove on completion

use opcode_table;

/// Values that represent additional flags of a decoded instruction.
#[repr(u32)]
#[derive(Copy)]
pub enum InstructionFlags {
    None_                   = 0x00000000,
    /// The instruction was decoded in 16 bit disassembler mode.
    DisassemblerMode16      = 0x00000001,
    /// The instruction was decoded in 32 bit disassembler mode.
    DisassemblerMode32      = 0x00000002,
    /// The instruction was decoded in 64 bit disassembler mode.
    DisassemblerMode64      = 0x00000004,
    /// The instruction has a segment prefix (0x26, 0x2E, 0x36, 0x3E, 0x64, 0x65).
    PrefixSegment           = 0x00000008, 
    /// The instruction has a lock prefix (0xF0).
    PrefixLock              = 0x00000010,  
    /// The instruction has a repne prefix (0xF2).
    PrefixRepne             = 0x00000020,  
    /// The instruction has a rep prefix (0xF3).   
    PrefixRep               = 0x00000040,  
    /// The instruction has an operand size prefix (0x66).  
    PrefixOperandSize       = 0x00000080, 
    /// The instruction has an address size prefix (0x67).   
    PrefixAddressSize       = 0x00000100, 
    /// The instruction has a rex prefix (0x40 - 0x4F).  
    PrefixRex               = 0x00000200, 
    /// The instruction has a vex prefix (0xC4 or 0xC5).  
    PrefixVex               = 0x00000400,
    /// The instruction has a modrm byte.  
    Modrm                   = 0x00000800,
    /// The instruction has a sib byte.  
    Sib                     = 0x00001000,
    /// The instruction has an operand with a relative address.
    Relative                = 0x00002000,
    /// An error occured while decoding the instruction.  
    ErrorMask               = 0xFFF00000,
    /// End of input reached while decoding the instruction.  
    ErrorEndOfInput         = 0x00100000,
    /// The instruction length has exceeded the maximum of 15 bytes.  
    ErrorLength             = 0x00200000,
    /// The instruction is invalid.
    ErrorInvalid            = 0x00400000,
    /// The instruction is invalid in 64 bit mode.   
    ErrorInvalid64          = 0x00800000,
    /// An error occured while decoding the instruction operands.  
    ErrorOperand            = 0x01000000
}

/// Values that represent a cpu register.
#[repr(u16)]
#[derive(Copy)]
pub enum Register {
    None,
    /* 8 bit general purpose registers */
    AL,     CL,     DL,     BL,
    AH,     CH,     DH,     BH,
    SPL,    BPL,    SIL,    DIL,
    R8B,    R9B,    R10B,   R11B,
    R12B,   R13B,   R14B,   R15B,
    /* 16 bit general purpose registers */
    AX,     CX,     DX,     BX,
    SP,     BP,     SI,     DI,
    R8W,    R9W,    R10W,   R11W,
    R12W,   R13W,   R14W,   R15W,
    /* 32 bit general purpose registers */
    EAX,    ECX,    EDX,    EBX,
    ESP,    EBP,    ESI,    EDI,
    R8D,    R9D,    R10D,   R11D,
    R12D,   R13D,   R14D,   R15D,
    /* 64 bit general purpose registers */
    RAX,    RCX,    RDX,    RBX,
    RSP,    RBP,    RSI,    RDI,
    R8,     R9,     R10,    R11,
    R12,    R13,    R14,    R15,
    /* segment registers */
    ES,     CS,     SS,     
    DS,     FS,     GS,
    /* control registers */
    CR0,    CR1,    CR2,    CR3,
    CR4,    CR5,    CR6,    CR7,
    CR8,    CR9,    CR10,   CR11,
    CR12,   CR13,   CR14,   CR15,
    /* debug registers */
    DR0,    DR1,    DR2,    DR3,
    DR4,    DR5,    DR6,    DR7,
    DR8,    DR9,    DR10,   DR11,
    DR12,   DR13,   DR14,   DR15,
    /* mmx registers */
    MM0,    MM1,    MM2,    MM3,
    MM4,    MM5,    MM6,    MM7,
    /* x87 registers */
    ST0,    ST1,    ST2,    ST3,
    ST4,    ST5,    ST6,    ST7,
    /* extended multimedia registers */
    XMM0,   XMM1,   XMM2,   XMM3,
    XMM4,   XMM5,   XMM6,   XMM7,
    XMM8,   XMM9,   XMM10,  XMM11,
    XMM12,  XMM13,  XMM14,  XMM15,
    /* 256 bit multimedia registers */
    YMM0,   YMM1,   YMM2,   YMM3,
    YMM4,   YMM5,   YMM6,   YMM7,
    YMM8,   YMM9,   YMM10,  YMM11,
    YMM12,  YMM13,  YMM14,  YMM15,
    /* instruction pointer register */
    RIP
}

/// Values that represent the type of a decoded operand.
#[repr(u8)]
#[derive(Copy)]
pub enum OperandType {
    /// The operand is not used.
    None,
    /// The operand is a register operand.
    Register,
    /// The operand is a memory operand.
    Memory,
    /// The operand is a pointer operand.
    Pointer,
    /// The operand is an immediate operand.
    Immediate,
    /// The operand is a relative immediate operand.
    RelImmediate,
    /// The operand is a constant value.
    Constant
}

/// Values that represent the operand access mode.
#[repr(u8)]
#[derive(Copy)]
pub enum OperandAccessMode {
    Na,
    /// The operand is accessed in read-only mode.
    Read,
    /// The operand is accessed in write mode.
    Write,
    /// The operand is accessed in read-write mode.
    ReadWrite
}

/// This struct holds information about a decoded operand.
#[repr(C)]
#[derive(Copy)]
pub struct OperandInfo {
    /// The type of the operand.
    /// @see OperandType
    pub type_: u8,
    /// The size of the operand.
    pub size: u16,
    /// The operand access mode.
    /// @see OperandAccessMode
    pub access_mode: u8,
    /// The base register.
    /// @see Register
    pub base: u16,
    /// The index register.
    /// @see Register
    pub index: u16,
    /// The scale factor.
    pub scale: u8,
    /// The lvalue offset. If the @c offset is zero and the operand @c type is not 
    ///  @c CONSTANT, no lvalue is present.
    pub offset: u8,
    /// Signals, if the lval is signed.
    pub signed_lval: bool,
    /// The lvalue.
    pub lval: u64 
}

impl OperandInfo {
    pub fn lval_sbyte(self) -> i8 {
        (self.lval & 0xFF) as i8
    }

    pub fn lval_ubyte(self) -> u8 {
        (self.lval & 0xFF) as u8
    }

    pub fn lval_sword(self) -> i16 {
        (self.lval & 0xFFFF) as i16
    }

    pub fn lval_uword(self) -> u16 {
        (self.lval & 0xFFFF) as u16
    }

    pub fn lval_sdword(self) -> i32 {
        (self.lval & 0xFFFFFFFF) as i32
    }

    pub fn lval_udword(self) -> u32 {
        (self.lval & 0xFFFFFFFF) as u32
    }

    pub fn lval_sqword(self) -> i64 {
        self.lval as i64
    }

    pub fn lval_uqword(self) -> u64 {
        self.lval
    }

    pub fn lval_ptr_seg(self) -> u16 {
        (self.lval & 0xFFFF) as u16
    }

    pub fn lval_ptr_off(self) -> u32 {
        ((self.lval >> 16) & 0xFFFFFFFF) as u32
    }
}

/// This struct holds information about a decoded instruction.
#[repr(C)]
#[allow(missing_copy_implementations)]
pub struct InstructionInfo {
    /// The instruction flags.
    pub flags: u32,
    /// The instruction mnemonic.
    /// @see InstructionMnemonic
    pub mnemonic: u16,
    /// The total length of the instruction.
    pub length: u8,
    /// Contains all bytes of the instruction.
    pub data: [u8; 15],
    /// The length of the instruction opcodes.
    pub opcode_length: u8,
    /// The instruction opcodes.
    pub opcode: [u8; 3],
    /// The operand mode.
    pub operand_mode: u8,
    /// The address mode.
    pub address_mode: u8,
    /// The decoded operands.
    pub operand: [OperandInfo; 4],
    /// The segment register. This value will default to @c NONE, if no segment register 
    /// prefix is present.
    /// @see Register
    pub segment: u16,
    /// The rex prefix byte.
    pub rex: u8,
    /// When 1, a 64-bit operand size is used. Otherwise, when 0, the default operand size 
    /// is used.
    pub rex_w: u8,
    /// This 1-bit value is an extension to the MODRM.reg field.
    pub rex_r: u8,
    /// This 1-bit value is an extension to the SIB.index field.
    pub rex_x: u8,
    /// This 1-bit value is an extension to the MODRM.rm field or the SIB.base field.
    pub rex_b: u8,
    /// The modrm byte.
    pub modrm: u8,
    /// The modrm modus bits. When this field is b11, then register-direct addressing mode 
    /// is used, otherwise register-indirect addressing mode is used. 
    pub modrm_mod: u8,
    /// The modrm register bits. The REX.R, VEX.~R or XOP.~R field can extend this field 
    /// with 1 most-significant bit to 4 bits total. 
    pub modrm_reg: u8,
    /// The extended modrm register bits. If the instruction definition does not have the
    /// @c IDF_ACCEPTS_REXR flag set, this value defaults to the normal @c modrm_reg 
    /// field.
    pub modrm_reg_ext: u8,
    /// The modrm register/memory bits. Specifies a direct or indirect register operand, 
    /// optionally with a displacement. The REX.B, VEX.~B or XOP.~B field can extend this 
    /// field with 1 most-significant bit to 4 bits total. 
    pub modrm_rm: u8,
    /// The extended modrm register/memory bits. If the instruction definition does not 
    /// have the @c IDF_ACCEPTS_REXB flag set, this value defaults to the normal 
    /// @c modrm_rm field.
    pub modrm_rm_ext: u8,
    /// The sib byte.
    pub sib: u8,
    /// This field indicates the scaling factor of SIB.index.
    pub sib_scale: u8,
    /// The index register to use. The REX.X, VEX.~X or XOP.~X field can extend this field 
    /// with 1 most-significant bit to 4 bits total.
    pub sib_index: u8,
    /// The extended index register. If the instruction definition does not have the
    /// @c IDF_ACCEPTS_REXX flag set, this value defaults to the normal @c sib_index 
    /// field.
    pub sib_index_ext: u8,
    /// The base register to use. The REX.B, VEX.~B or XOP.~B field can extend this field 
    /// with 1 most-significant bit to 4 bits total.
    pub sib_base: u8,
    /// The extended base register. If the instruction definition does not have the
    /// @c IDF_ACCEPTS_REXB flag set, this value defaults to the normal @c sib_index 
    /// field.
    pub sib_base_ext: u8,
    /// The primary vex prefix byte.
    pub vex_op: u8,
    /// The second vex prefix byte.
    pub vex_b1: u8,
    /// The third vex prefix byte.
    pub vex_b2: u8,
    /// This 1-bit value is an 'inverted' extension to the MODRM.reg field. The inverse of 
    /// REX.R.
    pub vex_r: u8,
    /// This 1-bit value is an 'inverted' extension to the SIB.index field. The inverse of 
    /// REX.X.
    pub vex_x: u8,
    /// This 1-bit value is an 'inverted' extension to the MODRM.rm field or the SIB.base 
    /// field. The inverse of REX.B.
    pub vex_b: u8,
    /// Specifies the opcode map to use. 
    /// 00 = 0x0F
    /// 01 = 0x0F 0x38
    /// 02 = 0x0F 0x3A
    pub vex_m_mmmm: u8,
    /// For integer instructions: when 1, a 64-bit operand size is used, otherwise, 
    /// when 0, the default operand size is used (equivalent with REX.W). For non-integer 
    /// instructions, this bit is a general opcode extension bit.
    pub vex_w: u8,
    /// An additional operand for the instruction. The value of the XMM or YMM register 
    /// is 'inverted'.
    pub vex_vvvv: u8,
    /// When 0, a 128-bit vector lengh is used. Otherwise, when 1, a 256-bit vector length 
    /// is used. 
    pub vex_l: u8,
    /// Specifies an implied mandatory prefix for the opcode.
    /// 00 = none
    /// 01 = 0x66
    /// 10 = 0xF3
    /// 11 = 0xF2
    pub vex_pp: u8,
    /// The effectively used REX/VEX.w value. If the instruction definition does not have 
    /// the @c IDF_ACCEPTS_REXW flag set, this value defaults to zero.
    pub eff_rexvex_w: u8,
    /// The effectively used REX/VEX.r value. If the instruction definition does not have 
    /// the @c IDF_ACCEPTS_REXR flag set, this value defaults to zero.
    pub eff_rexvex_r: u8,
    /// The effectively used REX/VEX.x value. If the instruction definition does not have 
    /// the @c IDF_ACCEPTS_REXX flag set, this value defaults to zero.
    pub eff_rexvex_x: u8,
    /// The effectively used REX/VEX.b value. If the instruction definition does not have 
    /// the @c IDF_ACCEPTS_REXB flag set, this value defaults to zero.
    pub eff_rexvex_b: u8,
    /// The effectively used VEX.l value. If the instruction definition does not have 
    /// the @c IDF_ACCEPTS_VEXL flag set, this value defaults to zero.
    pub eff_vex_l: u8,
    /// The instruction definition.
    pub instr_definition: *const opcode_table::InstructionDefinition,
    /// The instruction address points to the current instruction (relative to the initial 
    /// instruction pointer).
    pub instr_address: u64,
    /// The instruction pointer points to the address of the next instruction (relative
    /// to the initial instruction pointer). 
    /// This field is used to properly format relative instructions.         
    pub instr_pointer: u64
}
