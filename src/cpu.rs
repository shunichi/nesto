pub mod disasm;
mod instructions;
use self::disasm::disasm_one_inst_and_status;
pub use self::instructions::InstProp;
use self::instructions::*;
use crate::bus::Bus;
use crate::utils::{crossing_page_cycle, offset_addr, wrap_add16, wrap_sub8};

struct Flags {
    carry: bool,
    zero: bool,
    intrrupt: bool,
    decimal: bool,
    brk: bool,
    unused: bool,
    overflow: bool,
    negative: bool,
}

pub const FLG_C: u8 = 1; // carry
pub const FLG_Z: u8 = 1 << 1; // zero
pub const FLG_I: u8 = 1 << 2; // intrrupt
pub const FLG_D: u8 = 1 << 3; // decimal
pub const FLG_B: u8 = 1 << 4; // brk
pub const FLG_U: u8 = 1 << 5; // unused
pub const FLG_V: u8 = 1 << 6; // overflow
pub const FLG_N: u8 = 1 << 7; // negative

impl Flags {
    fn new() -> Flags {
        // 0x34
        Flags {
            carry: false,
            zero: false,
            intrrupt: true,
            decimal: false,
            brk: false,
            unused: true,
            overflow: false,
            negative: false,
        }
    }

    pub fn value(&self) -> u8 {
        let mut value: u8 = 0;
        if self.carry {
            value |= FLG_C;
        }
        if self.zero {
            value |= FLG_Z;
        }
        if self.intrrupt {
            value |= FLG_I;
        }
        if self.decimal {
            value |= FLG_D;
        }
        if self.brk {
            value |= FLG_B;
        }
        if self.unused {
            value |= FLG_U;
        }
        if self.overflow {
            value |= FLG_V;
        }
        if self.negative {
            value |= FLG_N;
        }
        value
    }

    pub fn set(&mut self, value: u8) {
        self.carry = (value & FLG_C) != 0;
        self.zero = (value & FLG_Z) != 0;
        self.intrrupt = (value & FLG_I) != 0;
        self.decimal = (value & FLG_D) != 0;
        self.brk = (value & FLG_B) != 0;
        self.overflow = (value & FLG_V) != 0;
        self.negative = (value & FLG_N) != 0;
    }
}
// http://wiki.nesdev.com/w/index.php/CPU_registers
pub struct Cpu {
    pub pc: u16,
    pub s: u8,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    flags: Flags,
    cycles: u8,
    elapsed_cycles: u64,
}

// https://wiki.nesdev.com/w/index.php/CPU_power_up_state
impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            pc: 0,
            s: 0xfd,
            a: 0,
            x: 0,
            y: 0,
            flags: Flags::new(),
            cycles: 0,
            elapsed_cycles: 7,
        }
    }

    pub fn clock(&mut self, bus: &mut Bus) {
        if self.cycles == 0 {
            disasm_one_inst_and_status(bus, self.pc, self);
            self.cycles = self.exec_inst(bus);
            self.elapsed_cycles += self.cycles as u64;
        }
        self.cycles -= 1;
    }

    pub fn carry_value(&self) -> u8 {
        if self.flags.carry {
            1
        } else {
            0
        }
    }

    fn compare(&mut self, v0: u8, v1: u8) {
        self.set_zn_flags(wrap_sub8(v0, v1));
        self.flags.carry = v0 >= v1;
    }

    fn set_flags(&mut self, prev_value: u8, data: u8, new_value: u16) {
        let new_u8_value = new_value as u8;
        self.set_zn_flags(new_u8_value);
        self.flags.carry = (new_value & 0x100) != 0;
        self.flags.overflow = (((prev_value ^ new_u8_value) & !(prev_value ^ data)) & 0x80) != 0;
    }

    fn set_z_flag(&mut self, new_value: u8) {
        self.flags.zero = new_value == 0;
    }

    fn set_n_flag(&mut self, new_value: u8) {
        self.flags.negative = (new_value & 0x80) != 0;
    }

    fn set_zn_flags(&mut self, new_value: u8) {
        self.set_z_flag(new_value);
        self.set_n_flag(new_value);
    }

    fn push(&mut self, bus: &mut Bus, value: u8) {
        bus.write(0x100 + self.s as u16, value);
        self.s -= 1;
    }

    fn push16(&mut self, bus: &mut Bus, value: u16) {
        bus.write16(0x100 + self.s as u16 - 1, value);
        self.s -= 2;
    }

    fn pop(&mut self, bus: &mut Bus) -> u8 {
        self.s += 1;
        bus.read(0x100 + self.s as u16)
    }

    fn exec_inst(&mut self, bus: &mut Bus) -> u8 {
        let inst = bus.read(self.pc);
        let inst_prop = &INST_PROPS[inst as usize];

        let (next_pc_addr, additional_cycles) = (inst_prop.func)(self, inst_prop, bus);
        self.pc = next_pc_addr.unwrap_or_else(|| self.next_pc(inst_prop));
        // if let Some(addr) = next_pc_addr {
        //     self.pc = addr;
        // } else {
        //     self.step_pc_to_next(inst_prop);
        // }
        inst_prop.cycles + additional_cycles
    }

    fn next_pc(&self, inst_prop: &InstProp) -> u16 {
        wrap_add16(self.pc, 1 + inst_prop.addr_mode.operand_len() as u16)
    }

    fn step_pc_to_next(&mut self, inst_prop: &InstProp) {
        self.pc = self.next_pc(inst_prop);
    }

    fn fetch_addr(&mut self, inst_prop: &InstProp, bus: &mut Bus) -> (u16, u8) {
        match inst_prop.addr_mode {
            AddressingMode::A => (0, 0),
            AddressingMode::Abs => (bus.read16(self.pc + 1), 0),
            AddressingMode::AbsX => {
                let abs_addr = bus.read16(self.pc + 1);
                let addr = wrap_add16(abs_addr, self.x as u16);
                (addr, crossing_page_cycle(abs_addr, addr))
            }
            AddressingMode::AbsY => {
                let abs_addr = bus.read16(self.pc + 1);
                let addr = wrap_add16(abs_addr, self.y as u16);
                (addr, crossing_page_cycle(abs_addr, addr))
            }
            AddressingMode::Imm => (self.pc + 1, 0),
            AddressingMode::Impl => (0, 0),
            AddressingMode::Ind => {
                let addr = bus.read16(self.pc + 1);
                (bus.read16bug(addr), 0)
            }
            AddressingMode::XInd => {
                let zaddr = bus.read(self.pc + 1) as u16;
                let addr = bus.read16zero(zaddr + self.x as u16);
                (addr, 0)
            }
            AddressingMode::IndY => {
                let zaddr = bus.read(self.pc + 1) as u16;
                let abs_addr = bus.read16zero(zaddr);
                let addr = wrap_add16(abs_addr, self.y as u16);
                (addr, crossing_page_cycle(abs_addr, addr))
            }
            AddressingMode::Rel => {
                let next_addr = wrap_add16(self.pc, 2);
                let offset = bus.read(self.pc + 1);
                (offset_addr(next_addr, offset), 0)
            }
            AddressingMode::Zpg => {
                let zaddr = bus.read(self.pc + 1) as u16;
                (zaddr, 0)
            }
            AddressingMode::ZpgX => {
                let zaddr = bus.read(self.pc + 1) as u16;
                ((zaddr + self.x as u16) & 0xff, 0)
            }
            AddressingMode::ZpgY => {
                let zaddr = bus.read(self.pc + 1) as u16;
                ((zaddr + self.y as u16) & 0xff, 0)
            }
        }
    }

    fn fetch_data(&mut self, inst_prop: &InstProp, bus: &mut Bus) -> (u8, u8) {
        let (addr, additonal_cycle) = self.fetch_addr(inst_prop, bus);
        let data = bus.read(addr);
        (data, additonal_cycle)
    }

    pub fn reset(&mut self, bus: &Bus) {
        self.pc = bus.read16(0xfffc);
    }

    pub fn inst_name(inst: Inst) -> &'static str {
        inst.name()
    }

    pub fn addr_mode_name(addr_mode: AddressingMode) -> &'static str {
        addr_mode.name()
    }

    pub fn inst_table_len() -> usize {
        INST_PROPS.len()
    }

    pub fn inst_prop(byte: u8) -> &'static InstProp {
        let index: usize = byte as usize;
        if index < INST_PROPS.len() {
            &INST_PROPS[index]
        } else {
            &INST_PROPS[2]
        }
    }
}
