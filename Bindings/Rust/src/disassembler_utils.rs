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

#[allow(dead_code)] // TODO: remove on completion

mod raw_interface {     
    use disassembler_types;
    
    extern {
        //uint64_t VDECalcAbsoluteTarget(
        //  const VXInstructionInfo &info, const VXOperandInfo &operand);

        fn VDECalcAbsoluteTarget(
            info: *const disassembler_types::InstructionInfo, 
            operand: *const disassembler_types::OperandInfo) -> u64;
    } // extern
} // mod raw_interface