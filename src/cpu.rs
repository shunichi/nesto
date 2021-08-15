use crate::bus::Bus;
use crate::cartridge::Cartridge;

macro_rules! enum_str {
  (pub enum $name:ident {
    $($variant:ident),*,
  }) => {
    #[derive(Copy, Clone)]
    pub enum $name {
      $($variant),*
    }

    impl $name {
      pub fn name(&self) -> &'static str {
        match self {
          $($name::$variant => stringify!($variant)),*
        }
      }
    }
  };
}

enum_str! {
  pub enum Inst {
      ADC, // add with carry
      AND, // and (with accumulator)
      ASL, // arithmetic shift left
      BCC, // branch on carry clear
      BCS, // branch on carry set
      BEQ, // branch on equal (zero set)
      BIT, // bit test
      BMI, // branch on minus (negative set)
      BNE, // branch on not equal (zero clear)
      BPL, // branch on plus (negative clear)
      BRK, // break / interrupt
      BVC, // branch on overflow clear
      BVS, // branch on overflow set
      CLC, // clear carry
      CLD, // clear decimal
      CLI, // clear interrupt disable
      CLV, // clear overflow
      CMP, // compare (with accumulator)
      CPX, // compare with X
      CPY, // compare with Y
      DEC, // decrement
      DEX, // decrement X
      DEY, // decrement Y
      EOR, // exclusive or (with accumulator)
      INC, // increment
      INX, // increment X
      INY, // increment Y
      JMP, // jump
      JSR, // jump subroutine
      LDA, // load accumulator
      LDX, // load X
      LDY, // load Y
      LSR, // logical shift right
      NOP, // no operation
      ORA, // or with accumulator
      PHA, // push accumulator
      PHP, // push processor status (SR)
      PLA, // pull accumulator
      PLP, // pull processor status (SR)
      ROL, // rotate left
      ROR, // rotate right
      RTI, // return from interrupt
      RTS, // return from subroutine
      SBC, // subtract with carry
      SEC, // set carry
      SED, // set decimal
      SEI, // set interrupt disable
      STA, // store accumulator
      STX, // store X
      STY, // store Y
      TAX, // transfer accumulator to X
      TAY, // transfer accumulator to Y
      TSX, // transfer stack pointer to X
      TXA, // transfer X to accumulator
      TXS, // transfer X to stack pointer
      TYA, // transfer Y to accumulator
      XXX, // undefined
  }
}

enum_str! {
  pub enum AddressingMode {
    A,    // Accumlator
    Abs,  // Abosolute
    AbsX, // Absolute X-Indexed
    AbsY, // Absolute Y-Indexed
    Imm,  // Immediate
    Impl, // Implied
    Ind,  // Indirect
    XInd, // X-indexed Indirect
    IndY, // Indirect Y-Indexed
    Rel,  // Relative
    Zpg,  // Zero Page
    ZpgX, // X-Indexed Zero Page
    ZpgY, // Y-Indexed Zero Page
  }
}

impl AddressingMode {
    pub fn operand_len(&self) -> usize {
        match self {
            AddressingMode::A => 0,
            AddressingMode::Abs => 2,
            AddressingMode::AbsX => 2,
            AddressingMode::AbsY => 2,
            AddressingMode::Imm => 1,
            AddressingMode::Impl => 0,
            AddressingMode::Ind => 2,
            AddressingMode::XInd => 1,
            AddressingMode::IndY => 1,
            AddressingMode::Rel => 1,
            AddressingMode::Zpg => 1,
            AddressingMode::ZpgX => 1,
            AddressingMode::ZpgY => 1,
        }
    }
}

pub struct InstProp {
    pub inst: Inst,
    pub addr_mode: AddressingMode,
    pub cycles: u8,
}

macro_rules! build_inst_table {
  ($({$i:ident,$a:ident,$c:expr}),*,) => {
    [$(InstProp{ inst: Inst::$i, addr_mode: AddressingMode::$a, cycles: $c }),*,]
  }
}

// Additional Cycles がある命令
// ADC, ORA, AND, ...
// 分岐命令
// BPL, BMI, BVC, BVS
// JMP は固定サイクル
const INST_PROPS: [InstProp; 256] = build_inst_table! {
  // $00
  {BRK, Impl, 7},
  {ORA, XInd, 6},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {ORA, Zpg, 3},
  {ASL, Zpg, 5},
  {XXX, Impl, 1},
  {PHP, Impl, 3},
  {ORA, Imm, 2},
  {ASL, A, 2},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {ORA, Abs, 4},
  {ASL, Abs, 6},
  {XXX, Impl, 1},
  // $10
  {BPL, Rel, 2},
  {ORA, IndY, 5},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {ORA, ZpgX, 4},
  {ASL, ZpgX, 6},
  {XXX, Impl, 1},
  {CLC, Impl, 2},
  {ORA, AbsY, 4},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {ORA, AbsX, 4},
  {ASL, AbsX, 7},
  {XXX, Impl, 1},
  // 21
  {JSR, Abs, 6},
  {AND, XInd, 6},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {BIT, Zpg, 3},
  {AND, Zpg, 3},
  {ROL, Zpg, 5},
  {XXX, Impl, 1},
  {PLP, Impl, 4},
  {AND, Imm, 2},
  {ROL, A, 2},
  {XXX, Impl, 1},
  {BIT, Abs, 4},
  {AND, Abs, 4},
  {ROL, Abs, 6},
  {XXX, Impl, 1},
  // $30
  {BMI, Rel, 2},
  {AND, IndY, 5},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {AND, ZpgX, 4},
  {ROL, ZpgX, 6},
  {XXX, Impl, 1},
  {SEC, Impl, 2},
  {AND, AbsY, 4},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {AND, AbsX, 4},
  {ROL, AbsX, 7},
  {XXX, Impl, 1},
  // $40
  {RTI, Impl, 6},
  {EOR, XInd, 6},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {EOR, Zpg, 3},
  {LSR, Zpg, 5},
  {XXX, Impl, 1},
  {PHA, Impl, 3},
  {EOR, Imm, 2},
  {LSR, A, 2},
  {XXX, Impl, 1},
  {JMP, Abs, 3},
  {EOR, Abs, 4},
  {LSR, Abs, 6},
  {XXX, Impl, 1},
  // $50
  {BVC, Rel, 2},
  {EOR, IndY, 5},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {EOR, ZpgX, 4},
  {LSR, ZpgX, 6},
  {XXX, Impl, 1},
  {CLI, Impl, 2},
  {EOR, AbsY, 4},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {EOR, AbsX, 4},
  {LSR, AbsX, 7},
  {XXX, Impl, 1},
  // $60
  {RTS, Impl, 6},
  {ADC, XInd, 6},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {ADC, Zpg, 3},
  {ROR, Zpg, 5},
  {XXX, Impl, 1},
  {PLA, Impl, 4},
  {ADC, Imm, 2},
  {ROR, A, 2},
  {XXX, Impl, 1},
  {JMP, Ind, 5},
  {ADC, Abs, 4},
  {ROR, Abs, 6},
  {XXX, Impl, 1},
  // $70
  {BVS, Rel, 2},
  {ADC, IndY, 5},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {ADC, ZpgX, 4},
  {ROR, ZpgX, 6},
  {XXX, Impl, 1},
  {SEI, Impl, 2},
  {ADC, AbsY, 4},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {ADC, AbsX, 4},
  {ROR, AbsX, 7},
  {XXX, Impl, 1},
  // $80
  {XXX, Impl, 1},
  {STA, XInd, 6},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {STY, Zpg, 3},
  {STA, Zpg, 3},
  {STX, Zpg, 3},
  {XXX, Impl, 1},
  {DEY, Impl, 2},
  {XXX, Impl, 1},
  {TXA, Impl, 2},
  {XXX, Impl, 1},
  {STY, Abs, 4},
  {STA, Abs, 4},
  {STX, Abs, 4},
  {XXX, Impl, 1},
  // $90
  {BCC, Rel, 2},
  {STA, IndY, 6},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {STY, ZpgX, 4},
  {STA, ZpgX, 4},
  {STX, ZpgY, 4},
  {XXX, Impl, 1},
  {TYA, Impl, 2},
  {STA, AbsY, 5},
  {TXS, Impl, 2},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {STA, AbsX, 5},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  // $A0
  {LDY, Imm, 2},
  {LDA, XInd, 6},
  {LDX, Imm, 2},
  {XXX, Impl, 1},
  {LDY, Zpg, 3},
  {LDA, Zpg, 3},
  {LDX, Zpg, 3},
  {XXX, Impl, 1},
  {TAY, Impl, 2},
  {LDA, Imm, 2},
  {TAX, Impl, 2},
  {XXX, Impl, 1},
  {LDY, Abs, 4},
  {LDA, Abs, 4},
  {LDX, Abs, 4},
  {XXX, Impl, 1},
  // $B0
  {BCS, Rel, 2},
  {LDA, IndY, 5},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {LDY, ZpgX, 4},
  {LDA, ZpgX, 4},
  {LDX, ZpgY, 4},
  {XXX, Impl, 1},
  {CLV, Impl, 2},
  {LDA, AbsY, 4},
  {TSX, Impl, 2},
  {XXX, Impl, 1},
  {LDY, AbsX, 4},
  {LDA, AbsX, 4},
  {LDX, AbsY, 4},
  {XXX, Impl, 1},
  // $C0
  {CPY, Imm, 2},
  {CMP, XInd, 6},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {CPY, Zpg, 3},
  {CMP, Zpg, 3},
  {DEC, Zpg, 5},
  {XXX, Impl, 1},
  {INY, Impl, 2},
  {CMP, Imm, 2},
  {DEX, Impl, 2},
  {XXX, Impl, 1},
  {CPY, Abs, 4},
  {CMP, Abs, 4},
  {DEC, Abs, 6},
  {XXX, Impl, 1},
  // $D0
  {BNE, Rel, 2},
  {CMP, IndY, 5},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {CMP, ZpgX, 4},
  {DEC, ZpgX, 6},
  {XXX, Impl, 1},
  {CLD, Impl, 2},
  {CMP, AbsY, 4},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {CMP, AbsX, 4},
  {DEC, AbsX, 7},
  {XXX, Impl, 1},
  // $E0
  {CPX, Imm, 2},
  {SBC, XInd, 6},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {CPX, Zpg, 3},
  {SBC, Zpg, 3},
  {INC, Zpg, 5},
  {XXX, Impl, 1},
  {INX, Impl, 2},
  {SBC, Imm, 2},
  {NOP, Impl, 2},
  {XXX, Impl, 1},
  {CPX, Abs, 4},
  {SBC, Abs, 4},
  {INC, Abs, 6},
  {XXX, Impl, 1},
  // $F0
  {BEQ, Rel, 2},
  {SBC, IndY, 5},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {SBC, ZpgX, 4},
  {INC, ZpgX, 6},
  {XXX, Impl, 1},
  {SED, Impl, 2},
  {SBC, AbsY, 4},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {XXX, Impl, 1},
  {SBC, AbsX, 4},
  {INC, AbsX, 7},
  {XXX, Impl, 1},
  // 		$100
};

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

const FLG_C: u8 = 1;
const FLG_Z: u8 = 1 << 1;
const FLG_I: u8 = 1 << 2;
const FLG_D: u8 = 1 << 3;
const FLG_B: u8 = 1 << 4;
const FLG_U: u8 = 1 << 5; // unused
const FLG_V: u8 = 1 << 6;
const FLG_N: u8 = 1 << 7;

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

    fn value(&self) -> u8 {
        let mut v: u8 = 0;
        if self.carry {
            v |= FLG_C;
        }
        if self.zero {
            v |= FLG_Z;
        }
        if self.intrrupt {
            v |= FLG_I;
        }
        if self.decimal {
            v |= FLG_D;
        }
        if self.brk {
            v |= FLG_B;
        }
        if self.overflow {
            v |= FLG_V;
        }
        if self.negative {
            v |= FLG_N;
        }
        v
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
        }
    }

    pub fn clock(&mut self, bus: &mut Bus) {
        if self.cycles == 0 {
            self.cycles = self.exec_inst(bus);
        } else {
            self.cycles -= 1;
        }
    }

    pub fn carry_value(&self) -> u8 {
        if self.flags.carry {
            1
        } else {
            0
        }
    }

    fn set_flags(&mut self, prev_value: u8, data: u8, new_value: u16) {
        let new_u8_value = (new_value & 0xff) as u8;
        self.flags.carry = (new_value & 0x100) != 0;
        self.flags.negative = (new_u8_value & 0x80) != 0;
        self.flags.zero = new_u8_value == 0;
        self.flags.overflow = (((prev_value ^ new_u8_value) & !(prev_value ^ data)) & 0x80) != 0;
    }

    fn exec_inst(&mut self, bus: &mut Bus) -> u8 {
        let inst = bus.read(self.pc);
        self.pc = ((self.pc as u32 + 1) & 0xffff) as u16;
        let inst_prop = &INST_PROPS[inst as usize];
        let (data, mut additional_cycle) = self.fetch_data(inst_prop, bus);

        match inst_prop.inst {
            Inst::ADC => {
                let prev_value = self.a;
                let new_value = self.a as u16 + data as u16 + self.carry_value() as u16;
                self.set_flags(prev_value, data as u8, new_value);
                self.a = (new_value & 0xff) as u8;
            }
            Inst::JMP => {
                self.pc = data;
            }
            _ => {}
        }
        inst_prop.cycles + additional_cycle
    }

    fn fetch_data(&mut self, inst_prop: &InstProp, bus: &mut Bus) -> (u16, u8) {
        match inst_prop.addr_mode {
            AddressingMode::Impl => (0, 0),
            AddressingMode::Imm => {
                let data = bus.read(self.pc);
                self.pc += 1;
                (data as u16, 0)
            }
            AddressingMode::Abs => {
                let data = bus.read16(self.pc);
                self.pc += 2;
                (data, 0)
            }
            _ => (0, 0),
        }
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
