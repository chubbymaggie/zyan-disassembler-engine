/*
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

extern crate libc;

// ============================================================================================= //
// Enums                                                                                         //
// ============================================================================================= //

/// Values that represent a disassembler mode.
#[repr(u8)]
#[derive(Copy)]
pub enum DisassemblerMode {
    M16Bit,
    M32Bit,
    M64Bit
}

/// Values that represent an instruction-set vendor.
#[repr(u8)]
#[derive(Copy)]
pub enum InstructionSetVendor {
    Any_,
    Intel,
    AMD
}

// ============================================================================================= //
// Raw interface                                                                                 //
// ============================================================================================= //

mod raw_interface {
    use super::libc::{c_void, size_t};
    use super::{DisassemblerMode, InstructionSetVendor};
    use disassembler_types::InstructionInfo;

    // VXBaseDataSource ------------------------------------------------------------------------ //

    type VXBaseDataSourceContext = c_void;

    extern {
        fn VXBaseDataSource_Release(
            ctx: *mut VXBaseDataSourceContext);

        fn VXBaseDataSource_InputPeek(
            ctx: *mut VXBaseDataSourceContext, 
            info: *mut InstructionInfo) -> u8;

        fn VXBaseDataSource_InputNext(
            ctx: *mut VXBaseDataSourceContext,
            info: *mut InstructionInfo) -> u8;

        fn VXBaseDataSource_InputCurrent(
            ctx: *const VXBaseDataSourceContext) -> u8;

        fn VXBaseDataSource_IsEndOfInput(
            ctx: *const VXBaseDataSourceContext) -> bool;

        fn VXBaseDataSource_GetPosition(
            ctx: *const VXBaseDataSourceContext) -> u64;

        fn VXBaseDataSource_SetPosition(
            ctx: *mut VXBaseDataSourceContext,
            position: u64) -> bool;
    }

    // VXMemoryDataSource ---------------------------------------------------------------------- //

    extern {
        fn VXMemoryDataSource_Create(
            buffer: *const c_void,
            bufferLen: size_t) -> *mut VXBaseDataSourceContext;
    }

    // VXMemoryDataSource ---------------------------------------------------------------------- //

    type VXInstructionDecoderContext = c_void;

    extern {
        fn VXInstructionDecoder_Create() -> *mut VXInstructionDecoderContext;

        fn VXInsstructionDecoder_CreateEx(
            input: *mut VXBaseDataSourceContext,
            disassemblerMode: DisassemblerMode,
            preferredVendor: InstructionSetVendor,
            instructionPointer: u64) -> *mut VXInstructionDecoderContext;

        fn VXInstructionDecoder_Release(
            ctx: VXInstructionDecoderContext);

        fn VXInstructionDecoder_DecodeInstruction(
            ctx: *mut VXInstructionDecoderContext,
            info: *mut InstructionInfo) -> bool;
    }

    // ----------------------------------------------------------------------------------------- //

} // mod raw_interface

// ============================================================================================= //