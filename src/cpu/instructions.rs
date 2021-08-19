use super::{Cpu, FLG_B, FLG_U};
use crate::bus::Bus;
use crate::utils::{crossing_page_cycle, wrap_add8, wrap_sub8};

// https://www.masswerk.at/6502/6502_instruction_set.html

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
      // illegal instructions
      XXX, // undefined
      LAX,
      SAX,
      DCP,
      ISB,
      SLO,
      RLA,
      SRE,
      RRA,
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

type InstFunc = fn(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8);

pub struct InstProp {
    pub inst: Inst,
    pub func: InstFunc,
    pub addr_mode: AddressingMode,
    pub cycles: u8,
    pub additional_cycle: u8,
    pub illegal: bool,
}

impl InstProp {
    pub fn is_jump(&self) -> bool {
        self.inst == Inst::JMP || self.inst == Inst::JSR
    }
}

macro_rules! build_inst_table {
  ($({$i:ident,$f:ident,$a:ident,$c:expr,$ac:expr, $il:expr}),*,) => {
    [$(InstProp{ inst: Inst::$i, func: $f, addr_mode: AddressingMode::$a, cycles: $c, additional_cycle: $ac, illegal: $il }),*,]
  }
}

// Additional Cycles がある命令
// ADC, ORA, AND, ...
// 分岐命令
// BPL, BMI, BVC, BVS
// JMP は固定サイクル
pub const INST_PROPS: [InstProp; 256] = build_inst_table! {
  // $00
  {BRK, brk, Impl, 7, 0, false},
  {ORA, ora, XInd, 6, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {SLO, slo, XInd, 8, 0, true},
  {NOP, nop, Zpg, 3, 0, true},
  {ORA, ora, Zpg, 3, 0, false},
  {ASL, asl, Zpg, 5, 0, false},
  {SLO, slo, Zpg, 5, 0, true},
  {PHP, php, Impl, 3, 0, false},
  {ORA, ora, Imm, 2, 0, false},
  {ASL, asl, A, 2, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {NOP, nop, Abs, 4, 0, true},
  {ORA, ora, Abs, 4, 0, false},
  {ASL, asl, Abs, 6, 0, false},
  {SLO, slo, Abs, 6, 0, true},
  // $10
  {BPL, bpl, Rel, 2, 1, false},
  {ORA, ora, IndY, 5, 1, false},
  {XXX, xxx, Impl, 1, 0, true},
  {SLO, slo, IndY, 8, 0, true},
  {NOP, nop, ZpgX, 4, 0, true},
  {ORA, ora, ZpgX, 4, 0, false},
  {ASL, asl, ZpgX, 6, 0, false},
  {SLO, slo, ZpgX, 6, 0, true},
  {CLC, clc, Impl, 2, 0, false},
  {ORA, ora, AbsY, 4, 1, false},
  {NOP, nop, Impl, 2, 0, true},
  {SLO, slo, AbsY, 7, 0, true},
  {NOP, nop, AbsX, 4, 0, true},
  {ORA, ora, AbsX, 4, 1, false},
  {ASL, asl, AbsX, 7, 0, false},
  {SLO, slo, AbsX, 7, 0, true},
  // $20
  {JSR, jsr, Abs, 6, 0, false},
  {AND, and, XInd, 6, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {RLA, rla, XInd, 8, 0, true},
  {BIT, bit, Zpg, 3, 0, false},
  {AND, and, Zpg, 3, 0, false},
  {ROL, rol, Zpg, 5, 0, false},
  {RLA, rla, Zpg, 5, 0, true},
  {PLP, plp, Impl, 4, 0, false},
  {AND, and, Imm, 2, 0, false},
  {ROL, rol, A, 2, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {BIT, bit, Abs, 4, 0, false},
  {AND, and, Abs, 4, 0, false},
  {ROL, rol, Abs, 6, 0, false},
  {RLA, rla, Abs, 6, 0, true},
  // $30
  {BMI, bmi, Rel, 2, 1, false},
  {AND, and, IndY, 5, 1, false},
  {XXX, xxx, Impl, 1, 0, true},
  {RLA, rla, IndY, 8, 0, true},
  {NOP, nop, ZpgX, 4, 0, true},
  {AND, and, ZpgX, 4, 0, false},
  {ROL, rol, ZpgX, 6, 0, false},
  {RLA, rla, ZpgX, 6, 0, true},
  {SEC, sec, Impl, 2, 0, false},
  {AND, and, AbsY, 4, 1, false},
  {NOP, nop, Impl, 2, 0, true},
  {RLA, rla, AbsY, 7, 0, true},
  {NOP, nop, AbsX, 4, 0, true},
  {AND, and, AbsX, 4, 1, false},
  {ROL, rol, AbsX, 7, 0, false},
  {RLA, rla, AbsX, 7, 0, true},
  // $40
  {RTI, rti, Impl, 6, 0, false},
  {EOR, eor, XInd, 6, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {SRE, sre, XInd, 8, 0, true},
  {NOP, nop, Zpg, 3, 0, true},
  {EOR, eor, Zpg, 3, 0, false},
  {LSR, lsr, Zpg, 5, 0, false},
  {SRE, sre, Zpg, 5, 0, true},
  {PHA, pha, Impl, 3, 0, false},
  {EOR, eor, Imm, 2, 0, false},
  {LSR, lsr, A, 2, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {JMP, jmp, Abs, 3, 0, false},
  {EOR, eor, Abs, 4, 0, false},
  {LSR, lsr, Abs, 6, 0, false},
  {SRE, sre, Abs, 6, 0, true},
  // $50
  {BVC, bvc, Rel, 2, 1, false},
  {EOR, eor, IndY, 5, 1, false},
  {XXX, xxx, Impl, 1, 0, true},
  {SRE, sre, IndY, 8, 0, true},
  {NOP, nop, ZpgX, 4, 0, true},
  {EOR, eor, ZpgX, 4, 0, false},
  {LSR, lsr, ZpgX, 6, 0, false},
  {SRE, sre, ZpgX, 6, 0, true},
  {CLI, cli, Impl, 2, 0, false},
  {EOR, eor, AbsY, 4, 1, false},
  {NOP, nop, Impl, 2, 0, true},
  {SRE, sre, AbsY, 7, 0, true},
  {NOP, nop, AbsX, 4, 0, true},
  {EOR, eor, AbsX, 4, 1, false},
  {LSR, lsr, AbsX, 7, 0, false},
  {SRE, sre, AbsX, 7, 0, true},
  // $60
  {RTS, rts, Impl, 6, 0, false},
  {ADC, adc, XInd, 6, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {RRA, rra, XInd, 8, 0, true},
  {NOP, nop, Zpg, 3, 0, true},
  {ADC, adc, Zpg, 3, 0, false},
  {ROR, ror, Zpg, 5, 0, false},
  {RRA, rra, Zpg, 5, 0, true},
  {PLA, pla, Impl, 4, 0, false},
  {ADC, adc, Imm, 2, 0, false},
  {ROR, ror, A, 2, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {JMP, jmp, Ind, 5, 0, false},
  {ADC, adc, Abs, 4, 0, false},
  {ROR, ror, Abs, 6, 0, false},
  {RRA, rra, Abs, 6, 0, true},
  // $70
  {BVS, bvs, Rel, 2, 1, false},
  {ADC, adc, IndY, 5, 1, false},
  {XXX, xxx, Impl, 1, 0, true},
  {RRA, rra, IndY, 8, 0, true},
  {NOP, nop, ZpgX, 4, 0, true},
  {ADC, adc, ZpgX, 4, 0, false},
  {ROR, ror, ZpgX, 6, 0, false},
  {RRA, rra, ZpgX, 6, 0, true},
  {SEI, sei, Impl, 2, 0, false},
  {ADC, adc, AbsY, 4, 1, false},
  {NOP, nop, Impl, 2, 0, true},
  {RRA, rra, AbsY, 7, 0, true},
  {NOP, nop, AbsX, 4, 0, true},
  {ADC, adc, AbsX, 4, 1, false},
  {ROR, ror, AbsX, 7, 0, false},
  {RRA, rra, AbsX, 7, 0, true},
  // $80
  {NOP, nop, Imm, 2, 0, true},
  {STA, sta, XInd, 6, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {SAX, sax, XInd, 6, 0, true},
  {STY, sty, Zpg, 3, 0, false},
  {STA, sta, Zpg, 3, 0, false},
  {STX, stx, Zpg, 3, 0, false},
  {SAX, sax, Zpg, 3, 0, true},
  {DEY, dey, Impl, 2, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {TXA, txa, Impl, 2, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {STY, sty, Abs, 4, 0, false},
  {STA, sta, Abs, 4, 0, false},
  {STX, stx, Abs, 4, 0, false},
  {SAX, sax, Abs, 4, 0, true},
  // $90
  {BCC, bcc, Rel, 2, 1, false},
  {STA, sta, IndY, 6, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {XXX, xxx, Impl, 1, 0, true},
  {STY, sty, ZpgX, 4, 0, false},
  {STA, sta, ZpgX, 4, 0, false},
  {STX, stx, ZpgY, 4, 0, false},
  {SAX, sax, ZpgY, 4, 0, true},
  {TYA, tya, Impl, 2, 0, false},
  {STA, sta, AbsY, 5, 0, false},
  {TXS, txs, Impl, 2, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {XXX, xxx, Impl, 1, 0, true},
  {STA, sta, AbsX, 5, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {XXX, xxx, Impl, 1, 0, true},
  // $A0
  {LDY, ldy, Imm, 2, 0, false},
  {LDA, lda, XInd, 6, 0, false},
  {LDX, ldx, Imm, 2, 0, false},
  {LAX, lax, XInd, 6, 0, true},
  {LDY, ldy, Zpg, 3, 0, false},
  {LDA, lda, Zpg, 3, 0, false},
  {LDX, ldx, Zpg, 3, 0, false},
  {LAX, lax, Zpg, 3, 0, true},
  {TAY, tay, Impl, 2, 0, false},
  {LDA, lda, Imm, 2, 0, false},
  {TAX, tax, Impl, 2, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {LDY, ldy, Abs, 4, 0, false},
  {LDA, lda, Abs, 4, 0, false},
  {LDX, ldx, Abs, 4, 0, false},
  {LAX, lax, Abs, 4, 0, true},
  // $B0
  {BCS, bcs, Rel, 2, 1, false},
  {LDA, lda, IndY, 5, 1, false},
  {XXX, xxx, Impl, 1, 0, true},
  {LAX, lax, IndY, 5, 1, true},
  {LDY, ldy, ZpgX, 4, 0, false},
  {LDA, lda, ZpgX, 4, 0, false},
  {LDX, ldx, ZpgY, 4, 0, false},
  {LAX, lax, ZpgY, 4, 0, true},
  {CLV, clv, Impl, 2, 0, false},
  {LDA, lda, AbsY, 4, 1, false},
  {TSX, tsx, Impl, 2, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {LDY, ldy, AbsX, 4, 1, false},
  {LDA, lda, AbsX, 4, 1, false},
  {LDX, ldx, AbsY, 4, 1, false},
  {LAX, lax, AbsY, 4, 1, true},
  // $C0
  {CPY, cpy, Imm, 2, 0, false},
  {CMP, cmp, XInd, 6, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {DCP, dcp, XInd, 8, 0, true},
  {CPY, cpy, Zpg, 3, 0, false},
  {CMP, cmp, Zpg, 3, 0, false},
  {DEC, dec, Zpg, 5, 0, false},
  {DCP, dcp, Zpg, 5, 0, true},
  {INY, iny, Impl, 2, 0, false},
  {CMP, cmp, Imm, 2, 0, false},
  {DEX, dex, Impl, 2, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {CPY, cpy, Abs, 4, 0, false},
  {CMP, cmp, Abs, 4, 0, false},
  {DEC, dec, Abs, 6, 0, false},
  {DCP, dcp, Abs, 6, 0, true},
  // $D0
  {BNE, bne, Rel, 2, 1, false},
  {CMP, cmp, IndY, 5, 1, false},
  {XXX, xxx, Impl, 1, 0, true},
  {DCP, dcp, IndY, 8, 0, true},
  {NOP, nop, ZpgX, 4, 0, true},
  {CMP, cmp, ZpgX, 4, 0, false},
  {DEC, dec, ZpgX, 6, 0, false},
  {DCP, dcp, ZpgX, 6, 0, true},
  {CLD, cld, Impl, 2, 0, false},
  {CMP, cmp, AbsY, 4, 1, false},
  {NOP, nop, Impl, 2, 0, true},
  {DCP, dcp, AbsY, 7, 0, true},
  {NOP, nop, AbsX, 4, 0, true},
  {CMP, cmp, AbsX, 4, 1, false},
  {DEC, dec, AbsX, 7, 0, false},
  {DCP, dcp, AbsX, 7, 0, true},
  // $E0
  {CPX, cpx, Imm, 2, 0, false},
  {SBC, sbc, XInd, 6, 0, false},
  {XXX, xxx, Impl, 1, 0, true},
  {ISB, isb, XInd, 8, 0, true},
  {CPX, cpx, Zpg, 3, 0, false},
  {SBC, sbc, Zpg, 3, 0, false},
  {INC, inc, Zpg, 5, 0, false},
  {ISB, isb, Zpg, 5, 0, true},
  {INX, inx, Impl, 2, 0, false},
  {SBC, sbc, Imm, 2, 0, false},
  {NOP, nop, Impl, 2, 0, false},
  {SBC, sbc, Imm, 2, 0, true},
  {CPX, cpx, Abs, 4, 0, false},
  {SBC, sbc, Abs, 4, 0, false},
  {INC, inc, Abs, 6, 0, false},
  {ISB, isb, Abs, 6, 0, true},
  // $F0
  {BEQ, beq, Rel, 2, 1, false},
  {SBC, sbc, IndY, 5, 1, false},
  {XXX, xxx, Impl, 8, 0, true},
  {ISB, isb, IndY, 8, 0, true},
  {NOP, nop, ZpgX, 4, 0, true},
  {SBC, sbc, ZpgX, 4, 0, false},
  {INC, inc, ZpgX, 6, 0, false},
  {ISB, isb, ZpgX, 6, 0, true},
  {SED, sed, Impl, 2, 0, false},
  {SBC, sbc, AbsY, 4, 1, false},
  {NOP, nop, Impl, 2, 0, true},
  {ISB, isb, AbsY, 7, 0, true},
  {NOP, nop, AbsX, 4, 0, true},
  {SBC, sbc, AbsX, 4, 1, false},
  {INC, inc, AbsX, 7, 0, false},
  {ISB, isb, AbsX, 7, 0, true},
};

pub fn xxx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.step_pc_to_next(inst_prop);
    (None, 0)
}

pub fn adc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    let prev_value = cpu.a;
    let new_value = cpu.a as u16 + data as u16 + cpu.carry_value() as u16;
    cpu.set_flags(prev_value, data as u8, new_value);
    cpu.a = new_value as u8;
    (None, additional_cycle)
}

pub fn and(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    let new_value = cpu.a & data;
    cpu.a = new_value;
    cpu.set_zn_flags(new_value);
    (None, additional_cycle)
}

pub fn asl(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    if inst_prop.addr_mode == AddressingMode::A {
        cpu.flags.carry = (cpu.a & 0x80) != 0;
        cpu.a <<= 1;
        cpu.set_zn_flags(cpu.a);
    } else {
        let (addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
        let data = bus.read(addr);
        let new_value = data << 1;
        cpu.flags.carry = (data & 0x80) != 0;
        bus.write(addr, new_value);
        cpu.set_zn_flags(new_value);
    }
    (None, 0)
}

pub fn branch(
    cpu: &mut Cpu,
    inst_prop: &InstProp,
    bus: &mut Bus,
    do_branch: bool,
) -> (Option<u16>, u8) {
    let mut additional_cycles: u8 = 0;
    if do_branch {
        additional_cycles = 1;
        let (branch_addr, _) = cpu.fetch_addr(inst_prop, bus);
        additional_cycles += crossing_page_cycle(branch_addr, cpu.next_pc(inst_prop));
        (Some(branch_addr), additional_cycles)
    } else {
        (None, 0)
    }
}

pub fn bcc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    branch(cpu, inst_prop, bus, !cpu.flags.carry)
}

pub fn bcs(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    branch(cpu, inst_prop, bus, cpu.flags.carry)
}

pub fn beq(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    branch(cpu, inst_prop, bus, cpu.flags.zero)
}

pub fn bit(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (data, _additional_cycle) = cpu.fetch_data(inst_prop, bus);
    let tested = cpu.a & data;
    cpu.set_z_flag(tested);
    cpu.set_n_flag(data);
    cpu.flags.overflow = data & (1 << 6) != 0;
    (None, 0)
}

pub fn bmi(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    branch(cpu, inst_prop, bus, cpu.flags.negative)
}

pub fn bne(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    branch(cpu, inst_prop, bus, !cpu.flags.zero)
}

pub fn bpl(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    branch(cpu, inst_prop, bus, !cpu.flags.negative)
}

pub fn brk(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.push16(bus, cpu.pc + 2);
    cpu.push(bus, cpu.flags.value() | FLG_B | FLG_U);
    cpu.flags.brk = false;
    cpu.flags.intrrupt = true;
    cpu.pc = bus.read16(0xfffe);
    (None, 0)
}

pub fn bvc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    branch(cpu, inst_prop, bus, !cpu.flags.overflow)
}

pub fn bvs(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    branch(cpu, inst_prop, bus, cpu.flags.overflow)
}

pub fn clc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.flags.carry = false;
    (None, 0)
}

pub fn cld(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.flags.decimal = false;
    (None, 0)
}

pub fn cli(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.flags.intrrupt = false;
    (None, 0)
}

pub fn clv(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.flags.overflow = false;
    (None, 0)
}

pub fn cmp(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    cpu.compare(cpu.a, data);
    (None, additional_cycle)
}

pub fn cpx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    cpu.compare(cpu.x, data);
    (None, additional_cycle)
}

pub fn cpy(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    cpu.compare(cpu.y, data);
    (None, additional_cycle)
}

pub fn dec(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    let data = bus.read(addr);
    let result = wrap_sub8(data, 1);
    bus.write(addr, result);
    cpu.set_zn_flags(result);
    (None, 0)
}

pub fn dex(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.x = wrap_sub8(cpu.x, 1);
    cpu.set_zn_flags(cpu.x);
    (None, 0)
}

pub fn dey(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.y = wrap_sub8(cpu.y, 1);
    cpu.set_zn_flags(cpu.y);
    (None, 0)
}

pub fn eor(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    cpu.a = cpu.a ^ data;
    cpu.set_zn_flags(cpu.a);
    (None, additional_cycle)
}

pub fn inc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    let data = bus.read(addr);
    let result = wrap_add8(data, 1);
    bus.write(addr, result);
    cpu.set_zn_flags(result);
    (None, 0)
}

pub fn inx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.x = wrap_add8(cpu.x, 1);
    cpu.set_zn_flags(cpu.x);
    (None, 0)
}

pub fn iny(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.y = wrap_add8(cpu.y, 1);
    cpu.set_zn_flags(cpu.y);
    (None, 0)
}

pub fn jmp(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (branch_addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    (Some(branch_addr), 0)
}

pub fn jsr(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (branch_addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    let addr = cpu.pc + 2;
    cpu.push(bus, (addr >> 8) as u8);
    cpu.push(bus, (addr & 0xff) as u8);
    (Some(branch_addr), 0)
}

pub fn lda(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    cpu.a = data;
    cpu.set_zn_flags(data);
    (None, additional_cycle)
}

pub fn ldx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    cpu.x = data;
    cpu.set_zn_flags(data);
    (None, additional_cycle)
}

pub fn ldy(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    cpu.y = data;
    cpu.set_zn_flags(data);
    (None, additional_cycle)
}

pub fn lsr(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    if inst_prop.addr_mode == AddressingMode::A {
        cpu.flags.carry = (cpu.a & 0x01) != 0;
        cpu.a >>= 1;
        cpu.set_zn_flags(cpu.a);
    } else {
        let (addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
        let data = bus.read(addr);
        let new_value = data >> 1;
        cpu.flags.carry = (data & 0x01) != 0;
        bus.write(addr, new_value);
        cpu.set_zn_flags(new_value);
    }
    (None, 0)
}

pub fn nop(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (_addr, additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    (None, additional_cycle)
}

pub fn ora(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    cpu.a = cpu.a | data;
    cpu.set_zn_flags(cpu.a);
    (None, additional_cycle)
}

pub fn pha(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.push(bus, cpu.a);
    (None, 0)
}

pub fn php(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.push(bus, cpu.flags.value() | FLG_B | FLG_U);
    cpu.flags.brk = false;
    (None, 0)
}

pub fn pla(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.a = cpu.pop(bus);
    cpu.set_zn_flags(cpu.a);
    (None, 0)
}

pub fn plp(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let value = cpu.pop(bus);
    // Clear brk flag to get the same flags as nestest.log
    cpu.flags.set((value & !FLG_B) | FLG_U);
    (None, 0)
}

pub fn rol(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let bit0 = if cpu.flags.carry { 0x01 } else { 0 };
    if inst_prop.addr_mode == AddressingMode::A {
        cpu.flags.carry = (cpu.a & 0x80) != 0;
        cpu.a = (cpu.a << 1) | bit0;
        cpu.set_zn_flags(cpu.a);
    } else {
        let (addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
        let data = bus.read(addr);
        let new_value = (data << 1) | bit0;
        cpu.flags.carry = (data & 0x80) != 0;
        bus.write(addr, new_value);
        cpu.set_zn_flags(new_value);
    }
    (None, 0)
}

pub fn ror(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let bit7 = if cpu.flags.carry { 0x80 } else { 0 };
    if inst_prop.addr_mode == AddressingMode::A {
        cpu.flags.carry = (cpu.a & 0x01) != 0;
        cpu.a = (cpu.a >> 1) | bit7;
        cpu.set_zn_flags(cpu.a);
    } else {
        let (addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
        let data = bus.read(addr);
        let new_value = (data >> 1) | bit7;
        cpu.flags.carry = (data & 0x01) != 0;
        bus.write(addr, new_value);
        cpu.set_zn_flags(new_value);
    }
    (None, 0)
}

pub fn rti(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let s = cpu.pop(bus);
    cpu.flags.set((s & !FLG_B) | FLG_U);
    let pc_lo = cpu.pop(bus) as u16;
    let pc_hi = cpu.pop(bus) as u16;
    let pc = pc_hi << 8 | pc_lo;
    (Some(pc), 0)
}

pub fn rts(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let lo = cpu.pop(bus);
    let hi = cpu.pop(bus);
    let ret_addr = (hi as u16) << 8 | lo as u16;
    (Some(ret_addr + 1), 0)
}

pub fn sbc(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    let inverted = data ^ 0xff;
    let prev_value = cpu.a;
    let new_value = cpu.a as u16 + inverted as u16 + cpu.carry_value() as u16;

    // let new_u8_value = new_value as u8;
    // cpu.set_zn_flags(new_u8_value);
    // cpu.flags.carry = (new_value & 0x100) != 0;
    // cpu.flags.overflow = (((prev_value ^ new_u8_value) & (new_u8_value ^ inverted)) & 0x80) != 0;

    cpu.set_flags(prev_value, inverted, new_value);

    cpu.a = new_value as u8;
    (None, additional_cycle)
}

pub fn sec(cpu: &mut Cpu, inst_prop: &InstProp, _bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.flags.carry = true;
    (None, 0)
}

pub fn sed(cpu: &mut Cpu, inst_prop: &InstProp, _bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.flags.decimal = true;
    (None, 0)
}

pub fn sei(cpu: &mut Cpu, inst_prop: &InstProp, _bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.flags.intrrupt = true;
    (None, 0)
}

pub fn sta(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    bus.write(addr, cpu.a);
    (None, 0)
}

pub fn stx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    bus.write(addr, cpu.x);
    (None, 0)
}

pub fn sty(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    bus.write(addr, cpu.y);
    (None, 0)
}

pub fn tax(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.x = cpu.a;
    cpu.set_zn_flags(cpu.x);
    (None, 0)
}

pub fn tay(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.y = cpu.a;
    cpu.set_zn_flags(cpu.y);
    (None, 0)
}

pub fn tsx(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.x = cpu.s;
    cpu.set_zn_flags(cpu.x);
    (None, 0)
}

pub fn txa(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.a = cpu.x;
    cpu.set_zn_flags(cpu.a);
    (None, 0)
}

pub fn txs(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.s = cpu.x;
    (None, 0)
}

pub fn tya(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    cpu.a = cpu.y;
    cpu.set_zn_flags(cpu.a);
    (None, 0)
}

pub fn lax(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (data, additional_cycle) = cpu.fetch_data(inst_prop, bus);
    cpu.a = data;
    cpu.x = data;
    cpu.set_zn_flags(cpu.a);
    (None, additional_cycle)
}

pub fn sax(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    bus.write(addr, cpu.a & cpu.x);
    (None, 0)
}

pub fn dcp(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (addr, additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    let data = bus.read(addr);
    bus.write(addr, data);

    let tmp = wrap_sub8(data, 1);
    cpu.compare(cpu.a, tmp);
    bus.write(addr, tmp);
    (None, 0)
}

pub fn isb(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    let data = bus.read(addr);
    bus.write(addr, data);

    let tmp = wrap_add8(data, 1);
    let inverted = tmp ^ 0xff;
    let prev_value = cpu.a;
    let new_value = cpu.a as u16 + inverted as u16 + cpu.carry_value() as u16;
    let new_u8_value = new_value as u8;
    cpu.set_zn_flags(new_u8_value);
    cpu.flags.carry = (new_value & 0x100) != 0;
    cpu.flags.overflow = (((prev_value ^ tmp) & (prev_value ^ new_u8_value)) & 0x80) != 0;
    cpu.a = new_u8_value;
    bus.write(addr, tmp);
    (None, 0)
}

pub fn slo(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    let data = bus.read(addr);
    bus.write(addr, data);
    cpu.flags.carry = (data & 0x80) != 0;

    let tmp = data << 1;
    cpu.a |= tmp;
    cpu.set_zn_flags(cpu.a);
    bus.write(addr, tmp);
    (None, 0)
}

pub fn rla(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    let data = bus.read(addr);
    bus.write(addr, data);
    let prev_carry = cpu.carry_value();
    cpu.flags.carry = (data & 0x80) != 0;

    let tmp = data << 1 | prev_carry;
    cpu.a &= tmp;
    cpu.set_zn_flags(cpu.a);
    bus.write(addr, tmp);
    (None, 0)
}

pub fn sre(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    let data = bus.read(addr);
    bus.write(addr, data);
    cpu.flags.carry = (data & 0x01) != 0;

    let tmp = data >> 1;
    cpu.a ^= tmp;
    cpu.set_zn_flags(cpu.a);
    bus.write(addr, tmp);
    (None, 0)
}

pub fn rra(cpu: &mut Cpu, inst_prop: &InstProp, bus: &mut Bus) -> (Option<u16>, u8) {
    let (addr, _additional_cycle) = cpu.fetch_addr(inst_prop, bus);
    let data = bus.read(addr);
    bus.write(addr, data);
    let prev_carry = cpu.carry_value() << 7;
    cpu.flags.carry = (data & 0x01) != 0;

    let tmp = data >> 1 | prev_carry;
    let result = cpu.a as u16 + tmp as u16 + cpu.carry_value() as u16;
    cpu.flags.overflow = (!(cpu.a ^ tmp) & (cpu.a ^ result as u8) & 0x80) != 0;
    cpu.flags.carry = (result & 0x100) != 0;
    cpu.a = result as u8;
    cpu.set_zn_flags(cpu.a);
    bus.write(addr, tmp);
    (None, 0)
}
