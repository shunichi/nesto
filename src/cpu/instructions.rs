use super::{Cpu, FLG_B, FLG_U};
use crate::bus::Bus;
use crate::utils::crossing_page_cycle;

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
pub const INST_PROPS: [InstProp; 256] = build_inst_table! {
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

pub fn xxx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles
}

pub fn adc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    let prev_value = cpu.a;
    let new_value = cpu.a as u16 + data as u16 + cpu.carry_value() as u16;
    cpu.set_flags(prev_value, data as u8, new_value);
    cpu.a = new_value as u8;
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
}

pub fn and(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    let new_value = cpu.a & data;
    cpu.a = new_value;
    cpu.set_zn_flags(new_value);
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
}

pub fn asl(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    if inst_prop.addr_mode == AddressingMode::A {
        cpu.a = cpu.a << 1;
        cpu.set_zn_flags(cpu.a);
        cpu.step_pc_to_next(inst_prop);
        inst_prop.cycles
    } else {
        let (addr, additional_cycle) = cpu.fetch_addr(inst_prop, bus);
        let new_value = bus.read(addr) << 1;
        bus.write(addr, new_value);
        cpu.set_zn_flags(new_value);
        cpu.step_pc_to_next(inst_prop);
        inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
    }
}

pub fn branch(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus, do_branch: bool) -> u8 {
    let mut additional_cycles: u8 = 0;
    if do_branch {
        additional_cycles = 1;
        let (branch_addr, _) = cpu.fetch_addr(inst_prop, bus);
        additional_cycles += crossing_page_cycle(branch_addr, cpu.next_pc(inst_prop));
        cpu.pc = branch_addr;
    } else {
        cpu.step_pc_to_next(inst_prop);
    }
    inst_prop.cycles + additional_cycles
}
pub fn bcc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    branch(cpu, inst_prop, bus, !cpu.flags.carry)
}

pub fn bcs(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    branch(cpu, inst_prop, bus, cpu.flags.carry)
}

pub fn beq(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let mut additional_cycles: u8 = 0;
    if cpu.flags.zero {
        additional_cycles = 1;
        let (branch_addr, _) = cpu.fetch_addr(inst_prop, bus);
        additional_cycles += crossing_page_cycle(branch_addr, cpu.next_pc(inst_prop));
        cpu.pc = branch_addr;
    } else {
        cpu.step_pc_to_next(inst_prop);
    }
    inst_prop.cycles + additional_cycles
}

pub fn bit(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    let tested = cpu.a & data;
    cpu.set_z_flag(data);
    cpu.set_n_flag(tested);
    cpu.flags.overflow = tested & (1 << 6) != 0;
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
}

pub fn bmi(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let mut additional_cycles: u8 = 0;
    if cpu.flags.negative {
        additional_cycles = 1;
        let (branch_addr, _) = cpu.fetch_addr(inst_prop, bus);
        additional_cycles += crossing_page_cycle(branch_addr, cpu.next_pc(inst_prop));
        cpu.pc = branch_addr;
    } else {
        cpu.step_pc_to_next(inst_prop);
    }
    inst_prop.cycles + additional_cycles
}

pub fn bne(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let mut additional_cycles: u8 = 0;
    if !cpu.flags.zero {
        additional_cycles = 1;
        let (branch_addr, _) = cpu.fetch_addr(inst_prop, bus);
        additional_cycles += crossing_page_cycle(branch_addr, cpu.next_pc(inst_prop));
        cpu.pc = branch_addr;
    } else {
        cpu.step_pc_to_next(inst_prop);
    }
    inst_prop.cycles + additional_cycles
}

pub fn bpl(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let mut additional_cycles: u8 = 0;
    if !cpu.flags.negative {
        additional_cycles = 1;
        let (branch_addr, _) = cpu.fetch_addr(inst_prop, bus);
        additional_cycles += crossing_page_cycle(branch_addr, cpu.next_pc(inst_prop));
        cpu.pc = branch_addr;
    } else {
        cpu.step_pc_to_next(inst_prop);
    }
    inst_prop.cycles + additional_cycles
}

pub fn brk(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn bvc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let mut additional_cycles: u8 = 0;
    if !cpu.flags.overflow {
        additional_cycles = 1;
        let (branch_addr, _) = cpu.fetch_addr(inst_prop, bus);
        additional_cycles += crossing_page_cycle(branch_addr, cpu.next_pc(inst_prop));
        cpu.pc = branch_addr;
    } else {
        cpu.step_pc_to_next(inst_prop);
    }
    inst_prop.cycles + additional_cycles
}

pub fn bvs(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let mut additional_cycles: u8 = 0;
    if cpu.flags.overflow {
        additional_cycles = 1;
        let (branch_addr, _) = cpu.fetch_addr(inst_prop, bus);
        additional_cycles += crossing_page_cycle(branch_addr, cpu.next_pc(inst_prop));
        cpu.pc = branch_addr;
    } else {
        cpu.step_pc_to_next(inst_prop);
    }
    inst_prop.cycles + additional_cycles
}

pub fn clc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    cpu.flags.carry = false;
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles
}

pub fn cld(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn cli(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn clv(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn cmp(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    cpu.compare(cpu.a, data);
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles + (inst_prop.additional_cycle & additional_cycle)
}

pub fn cpx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn cpy(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn dec(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn dex(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn dey(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn eor(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn inc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn inx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn iny(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn jmp(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (addr, additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    cpu.pc = addr;
    inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
}

pub fn jsr(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (branch_addr, additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    let addr = cpu.pc + 2;
    cpu.push(bus, (addr >> 8) as u8);
    cpu.push(bus, (addr & 0xff) as u8);
    cpu.pc = branch_addr;
    inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
}

pub fn lda(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    cpu.a = data;
    cpu.set_zn_flags(data);
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
}

pub fn ldx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    cpu.x = data;
    cpu.set_zn_flags(data);
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
}

pub fn ldy(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    cpu.y = data;
    cpu.set_zn_flags(data);
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles + (additional_cycle & inst_prop.additional_cycle)
}

pub fn lsr(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn nop(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles
}

pub fn ora(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn pha(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn php(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    cpu.push(bus, cpu.flags.value() | FLG_B | FLG_U);
    cpu.flags.brk = false;
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles
}

pub fn pla(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    cpu.a = cpu.pop(bus);
    cpu.set_zn_flags(cpu.a);
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles
}

pub fn plp(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn rol(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn ror(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn rti(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn rts(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let lo = cpu.pop(bus);
    let hi = cpu.pop(bus);
    let ret_addr = (hi as u16) << 8 | lo as u16;
    cpu.pc = ret_addr + 1;
    inst_prop.cycles
}

pub fn sbc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn sec(cpu: &mut Cpu, inst_prop: &InstProp, _bus: &mut Bus) -> u8 {
    cpu.flags.carry = true;
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles
}

pub fn sed(cpu: &mut Cpu, inst_prop: &InstProp, _bus: &mut Bus) -> u8 {
    cpu.flags.decimal = true;
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles
}

pub fn sei(cpu: &mut Cpu, inst_prop: &InstProp, _bus: &mut Bus) -> u8 {
    cpu.flags.intrrupt = true;
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles
}

pub fn sta(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (addr, additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    bus.write(addr, cpu.a);
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles + (inst_prop.additional_cycle & additional_cycle)
}

pub fn stx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (addr, additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    bus.write(addr, cpu.x);
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles + (inst_prop.additional_cycle & additional_cycle)
}

pub fn sty(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    let (addr, additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    bus.write(addr, cpu.y);
    cpu.step_pc_to_next(inst_prop);
    inst_prop.cycles + (inst_prop.additional_cycle & additional_cycle)
}

pub fn tax(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn tay(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn tsx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn txa(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn txs(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}

pub fn tya(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> u8 {
    panic!("not implemented");
}
