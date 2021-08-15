use crate::bus::Bus;

macro_rules! enum_str {
  (pub enum $name:ident {
    $($variant:ident),*,
  }) => {
    #[derive(Copy, Clone, PartialEq, Eq)]
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
    Abs,  // Absolute
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

type InstFunc = fn(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8;

pub struct InstProp {
    pub inst: Inst,
    pub func: InstFunc,
    pub addr_mode: AddressingMode,
    pub cycles: u8,
    pub additional_cycle: u8,
}

macro_rules! build_inst_table {
  ($({$i:ident,$f:ident,$a:ident,$c:expr,$ac:expr}),*,) => {
    [$(InstProp{ inst: Inst::$i, func: $f, addr_mode: AddressingMode::$a, cycles: $c, additional_cycle: $ac }),*,]
  }
}

// Additional Cycles がある命令
// ADC, ORA, AND, ...
// 分岐命令
// BPL, BMI, BVC, BVS
// JMP は固定サイクル
const INST_PROPS: [InstProp; 256] = build_inst_table! {
  // $00
  {BRK, brk, Impl, 7, 0},
  {ORA, ora, XInd, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {ORA, ora, Zpg, 3, 0},
  {ASL, asl, Zpg, 5, 0},
  {XXX, xxx, Impl, 1, 0},
  {PHP, php, Impl, 3, 0},
  {ORA, ora, Imm, 2, 0},
  {ASL, asl, A, 2, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {ORA, ora, Abs, 4, 0},
  {ASL, asl, Abs, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  // $10
  {BPL, bpl, Rel, 2, 1},
  {ORA, ora, IndY, 5, 1},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {ORA, ora, ZpgX, 4, 0},
  {ASL, asl, ZpgX, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  {CLC, clc, Impl, 2, 0},
  {ORA, ora, AbsY, 4, 1},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {ORA, ora, AbsX, 4, 1},
  {ASL, asl, AbsX, 7, 0},
  {XXX, xxx, Impl, 1, 0},
  // $20
  {JSR, jsr, Abs, 6, 0},
  {AND, and, XInd, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {BIT, bit, Zpg, 3, 0},
  {AND, and, Zpg, 3, 0},
  {ROL, rol, Zpg, 5, 0},
  {XXX, xxx, Impl, 1, 0},
  {PLP, plp, Impl, 4, 0},
  {AND, and, Imm, 2, 0},
  {ROL, rol, A, 2, 0},
  {XXX, xxx, Impl, 1, 0},
  {BIT, bit, Abs, 4, 0},
  {AND, and, Abs, 4, 0},
  {ROL, rol, Abs, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  // $30
  {BMI, bmi, Rel, 2, 1},
  {AND, and, IndY, 5, 1},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {AND, and, ZpgX, 4, 0},
  {ROL, rol, ZpgX, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  {SEC, sec, Impl, 2, 0},
  {AND, and, AbsY, 4, 1},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {AND, and, AbsX, 4, 1},
  {ROL, rol, AbsX, 7, 0},
  {XXX, xxx, Impl, 1, 0},
  // $40
  {RTI, rti, Impl, 6, 0},
  {EOR, eor, XInd, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {EOR, eor, Zpg, 3, 0},
  {LSR, lsr, Zpg, 5, 0},
  {XXX, xxx, Impl, 1, 0},
  {PHA, pha, Impl, 3, 0},
  {EOR, eor, Imm, 2, 0},
  {LSR, lsr, A, 2, 0},
  {XXX, xxx, Impl, 1, 0},
  {JMP, jmp, Abs, 3, 0},
  {EOR, eor, Abs, 4, 0},
  {LSR, lsr, Abs, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  // $50
  {BVC, bvc, Rel, 2, 1},
  {EOR, eor, IndY, 5, 1},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {EOR, eor, ZpgX, 4, 0},
  {LSR, lsr, ZpgX, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  {CLI, cli, Impl, 2, 0},
  {EOR, eor, AbsY, 4, 1},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {EOR, eor, AbsX, 4, 1},
  {LSR, lsr, AbsX, 7, 0},
  {XXX, xxx, Impl, 1, 0},
  // $60
  {RTS, rts, Impl, 6, 0},
  {ADC, adc, XInd, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {ADC, adc, Zpg, 3, 0},
  {ROR, ror, Zpg, 5, 0},
  {XXX, xxx, Impl, 1, 0},
  {PLA, pla, Impl, 4, 0},
  {ADC, adc, Imm, 2, 0},
  {ROR, ror, A, 2, 0},
  {XXX, xxx, Impl, 1, 0},
  {JMP, jmp, Ind, 5, 0},
  {ADC, adc, Abs, 4, 0},
  {ROR, ror, Abs, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  // $70
  {BVS, bvs, Rel, 2, 1},
  {ADC, adc, IndY, 5, 1},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {ADC, adc, ZpgX, 4, 0},
  {ROR, ror, ZpgX, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  {SEI, sei, Impl, 2, 0},
  {ADC, adc, AbsY, 4, 1},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {ADC, adc, AbsX, 4, 1},
  {ROR, ror, AbsX, 7, 0},
  {XXX, xxx, Impl, 1, 0},
  // $80
  {XXX, xxx, Impl, 1, 0},
  {STA, sta, XInd, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {STY, sty, Zpg, 3, 0},
  {STA, sta, Zpg, 3, 0},
  {STX, stx, Zpg, 3, 0},
  {XXX, xxx, Impl, 1, 0},
  {DEY, dey, Impl, 2, 0},
  {XXX, xxx, Impl, 1, 0},
  {TXA, txa, Impl, 2, 0},
  {XXX, xxx, Impl, 1, 0},
  {STY, sty, Abs, 4, 0},
  {STA, sta, Abs, 4, 0},
  {STX, stx, Abs, 4, 0},
  {XXX, xxx, Impl, 1, 0},
  // $90
  {BCC, bcc, Rel, 2, 1},
  {STA, sta, IndY, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {STY, sty, ZpgX, 4, 0},
  {STA, sta, ZpgX, 4, 0},
  {STX, stx, ZpgY, 4, 0},
  {XXX, xxx, Impl, 1, 0},
  {TYA, tya, Impl, 2, 0},
  {STA, sta, AbsY, 5, 0},
  {TXS, txs, Impl, 2, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {STA, sta, AbsX, 5, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  // $A0
  {LDY, ldy, Imm, 2, 0},
  {LDA, lda, XInd, 6, 0},
  {LDX, ldx, Imm, 2, 0},
  {XXX, xxx, Impl, 1, 0},
  {LDY, ldy, Zpg, 3, 0},
  {LDA, lda, Zpg, 3, 0},
  {LDX, ldx, Zpg, 3, 0},
  {XXX, xxx, Impl, 1, 0},
  {TAY, tay, Impl, 2, 0},
  {LDA, lda, Imm, 2, 0},
  {TAX, tax, Impl, 2, 0},
  {XXX, xxx, Impl, 1, 0},
  {LDY, ldy, Abs, 4, 0},
  {LDA, lda, Abs, 4, 0},
  {LDX, ldx, Abs, 4, 0},
  {XXX, xxx, Impl, 1, 0},
  // $B0
  {BCS, bcs, Rel, 2, 1},
  {LDA, lda, IndY, 5, 1},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {LDY, ldy, ZpgX, 4, 0},
  {LDA, lda, ZpgX, 4, 0},
  {LDX, ldx, ZpgY, 4, 0},
  {XXX, xxx, Impl, 1, 0},
  {CLV, clv, Impl, 2, 0},
  {LDA, lda, AbsY, 4, 1},
  {TSX, tsx, Impl, 2, 0},
  {XXX, xxx, Impl, 1, 0},
  {LDY, ldy, AbsX, 4, 1},
  {LDA, lda, AbsX, 4, 1},
  {LDX, ldx, AbsY, 4, 1},
  {XXX, xxx, Impl, 1, 0},
  // $C0
  {CPY, cpy, Imm, 2, 0},
  {CMP, cmp, XInd, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {CPY, cpy, Zpg, 3, 0},
  {CMP, cmp, Zpg, 3, 0},
  {DEC, dec, Zpg, 5, 0},
  {XXX, xxx, Impl, 1, 0},
  {INY, iny, Impl, 2, 0},
  {CMP, cmp, Imm, 2, 0},
  {DEX, dex, Impl, 2, 0},
  {XXX, xxx, Impl, 1, 0},
  {CPY, cpy, Abs, 4, 0},
  {CMP, cmp, Abs, 4, 0},
  {DEC, dec, Abs, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  // $D0
  {BNE, bne, Rel, 2, 1},
  {CMP, cmp, IndY, 5, 1},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {CMP, cmp, ZpgX, 4, 0},
  {DEC, dec, ZpgX, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  {CLD, cld, Impl, 2, 0},
  {CMP, cmp, AbsY, 4, 1},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {CMP, cmp, AbsX, 4, 1},
  {DEC, dec, AbsX, 7, 0},
  {XXX, xxx, Impl, 1, 0},
  // $E0
  {CPX, cpx, Imm, 2, 0},
  {SBC, sbc, XInd, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {CPX, cpx, Zpg, 3, 0},
  {SBC, sbc, Zpg, 3, 0},
  {INC, inc, Zpg, 5, 0},
  {XXX, xxx, Impl, 1, 0},
  {INX, inx, Impl, 2, 0},
  {SBC, sbc, Imm, 2, 0},
  {NOP, nop, Impl, 2, 0},
  {XXX, xxx, Impl, 1, 0},
  {CPX, cpx, Abs, 4, 0},
  {SBC, sbc, Abs, 4, 0},
  {INC, inc, Abs, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  // $F0
  {BEQ, beq, Rel, 2, 1},
  {SBC, sbc, IndY, 5, 1},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {SBC, sbc, ZpgX, 4, 0},
  {INC, inc, ZpgX, 6, 0},
  {XXX, xxx, Impl, 1, 0},
  {SED, sed, Impl, 2, 0},
  {SBC, sbc, AbsY, 4, 1},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {XXX, xxx, Impl, 1, 0},
  {SBC, sbc, AbsX, 4, 1},
  {INC, inc, AbsX, 7, 0},
  {XXX, xxx, Impl, 1, 0},
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

fn xxx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    cpu.next_pc(inst_prop);
    inst_prop.cycles
}

fn adc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    let prev_value = cpu.a;
    let new_value = cpu.a as u16 + data as u16 + cpu.carry_value() as u16;
    cpu.set_flags(prev_value, data as u8, new_value);
    cpu.a = new_value as u8;
    cpu.next_pc(inst_prop);
    inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
}

fn and(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    let new_value = cpu.a & data;
    cpu.a = new_value;
    cpu.set_zn_flags(new_value);
    cpu.next_pc(inst_prop);
    inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
}

fn asl(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    if inst_prop.addr_mode == AddressingMode::A {
        cpu.a = cpu.a << 1;
        cpu.set_zn_flags(cpu.a);
        cpu.next_pc(inst_prop);
        inst_prop.cycles
    } else {
        let (addr, additional_cycle) = cpu.fetch_addr(inst_prop, bus);
        let new_value = bus.read(addr) << 1;
        bus.write(addr, new_value);
        cpu.set_zn_flags(new_value);
        cpu.next_pc(inst_prop);
        inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
    }
}

fn bcc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let mut additional_cycles: u8 = 0;
    if !cpu.flags.carry {
        additional_cycles = 1;
        let (branch_addr, _) = cpu.fetch_addr(inst_prop, bus);
        let next_addr = cpu.pc + 1 + inst_prop.addr_mode.operand_len() as u16;
        additional_cycles += crossing_page_cycle(branch_addr, next_addr);
        cpu.pc = branch_addr;
    } else {
        cpu.next_pc(inst_prop);
    }
    inst_prop.cycles + additional_cycles
}

fn bcs(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let mut additional_cycles: u8 = 0;
    if cpu.flags.carry {
        additional_cycles = 1;
        let (branch_addr, _) = cpu.fetch_addr(inst_prop, bus);
        let next_addr = cpu.pc + 1 + inst_prop.addr_mode.operand_len() as u16;
        additional_cycles += crossing_page_cycle(branch_addr, next_addr);
        cpu.pc = branch_addr;
    } else {
        cpu.next_pc(inst_prop);
    }
    inst_prop.cycles + additional_cycles
}

fn beq(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let mut additional_cycles: u8 = 0;
    if cpu.flags.zero {
        additional_cycles = 1;
        let (branch_addr, _) = cpu.fetch_addr(inst_prop, bus);
        let next_addr = cpu.pc + 1 + inst_prop.addr_mode.operand_len() as u16;
        additional_cycles += crossing_page_cycle(branch_addr, next_addr);
        cpu.pc = branch_addr;
    } else {
        cpu.next_pc(inst_prop);
    }
    inst_prop.cycles + additional_cycles
}

fn bit(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn bmi(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn bne(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let mut additional_cycles: u8 = 0;
    if !cpu.flags.zero {
        additional_cycles = 1;
        let (branch_addr, _) = cpu.fetch_addr(inst_prop, bus);
        let next_addr = cpu.pc + 1 + inst_prop.addr_mode.operand_len() as u16;
        additional_cycles += crossing_page_cycle(branch_addr, next_addr);
        cpu.pc = branch_addr;
    } else {
        cpu.next_pc(inst_prop);
    }
    inst_prop.cycles + additional_cycles
}

fn bpl(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn brk(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn bvc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn bvs(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn clc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    cpu.flags.carry = false;
    cpu.next_pc(inst_prop);
    inst_prop.cycles
}

fn cld(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn cli(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn clv(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn cmp(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn cpx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn cpy(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn dec(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn dex(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn dey(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn eor(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn inc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn inx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn iny(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn jmp(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (addr, additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    cpu.pc = addr;
    inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
}

fn jsr(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (branch_addr, additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    let addr = cpu.pc + 2;
    bus.write(0x100 + cpu.s as u16, (addr >> 8) as u8);
    cpu.s -= 1;
    bus.write(0x100 + cpu.s as u16, (addr & 0xff) as u8);
    cpu.s -= 1;
    cpu.pc = branch_addr;
    inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
}

fn lda(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    cpu.a = data;
    cpu.set_zn_flags(data);
    cpu.next_pc(inst_prop);
    inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
}

fn ldx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    cpu.x = data;
    cpu.set_zn_flags(data);
    cpu.next_pc(inst_prop);
    inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
}

fn ldy(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    cpu.y = data;
    cpu.set_zn_flags(data);
    cpu.next_pc(inst_prop);
    inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
}

fn lsr(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn nop(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    cpu.next_pc(inst_prop);
    inst_prop.cycles
}

fn ora(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn pha(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn php(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn pla(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn plp(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn rol(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn ror(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn rti(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn rts(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn sbc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn sec(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    cpu.flags.carry = true;
    cpu.next_pc(inst_prop);
    inst_prop.cycles
}

fn sed(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn sei(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn sta(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn stx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (addr, additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    bus.write(addr, cpu.x);
    cpu.next_pc(inst_prop);
    inst_prop.cycles + (inst_prop.additional_cycle & additional_cycle)
}

fn sty(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn tax(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn tay(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn tsx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn txa(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn txs(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn tya(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

fn crossing_page_cycle(addr0: u16, addr1: u16) -> u8 {
    if (addr0 & 0xff00) != (addr1 & 0xff00) {
        1
    } else {
        0
    }
}

fn wrap_add16(a: u16, b: u16) -> u16 {
    (std::num::Wrapping(a) + std::num::Wrapping(b)).0
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

    fn exec_inst(&mut self, bus: &mut Bus) -> u8 {
        let inst = bus.read(self.pc);
        let inst_prop = &INST_PROPS[inst as usize];

        println!("{:04X} {}", self.pc, inst_prop.inst.name());

        (inst_prop.func)(self, inst_prop, bus)
    }

    fn next_pc(&mut self, inst_prop: &InstProp) {
        self.pc = wrap_add16(self.pc, 1 + inst_prop.addr_mode.operand_len() as u16);
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
                let offset = bus.read(self.pc + 1) as u16;
                let next_addr = wrap_add16(self.pc, 2);
                if (offset & 0x80) == 0 {
                    (wrap_add16(next_addr, offset), 0)
                } else {
                    (wrap_add16(next_addr, 0xff00 | offset), 0)
                }
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
