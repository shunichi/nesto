use crate::cartridge::Cartridge;
use crate::mapper::Mapper;

// http://wiki.nesdev.com/w/index.php/CPU_registers

pub struct Cpu {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub s: u8,
    pub p: u8,
}

macro_rules! enum_str {
  (pub enum $name:ident {
    $($variant:ident),*,
  }) => {
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
const INST_PROPS: [InstProp; 128] = build_inst_table! {
  // 00
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
  // 10
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
  // 30
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
  // 40
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
  // 50
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
  // 60
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
  // 70
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
  // 80
};

// https://wiki.nesdev.com/w/index.php/CPU_power_up_state
impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            s: 0xfd,
            p: 0x34,
        }
    }

    pub fn reset<M: Mapper>(&mut self, mapper: &M, cartridge: &Cartridge) {
        self.pc = mapper.read16(cartridge, 0xfffc);
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
