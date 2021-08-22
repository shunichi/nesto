use crate::bus::Bus;
use crate::cpu::{AddressingMode, Cpu, InstProp};
use crate::utils::{offset_addr, wrap_add16};
use std::io::Write;

fn operand_hex_string(bus: &Bus, addr: u16, addr_mode: AddressingMode) -> String {
    match addr_mode.operand_len() {
        0 => "".to_owned(),
        1 => format!("{:02X}", bus.read(addr)),
        2 => format!(
            "{:02X} {:02X}",
            bus.read(addr),
            bus.read((addr as u32 + 1) as u16)
        ),
        _ => "".to_owned(),
    }
}

fn operand_string(bus: &Bus, addr: u16, addr_mode: AddressingMode) -> Option<String> {
    match addr_mode {
        AddressingMode::A => Some("A".to_owned()),
        AddressingMode::Abs => Some(format!("${:04X}", bus.read16(addr))),
        AddressingMode::AbsX => Some(format!("${:04X},x", bus.read16(addr))),
        AddressingMode::AbsY => Some(format!("${:04X},y", bus.read16(addr))),
        AddressingMode::Imm => Some(format!("#${:02X}", bus.read(addr))),
        AddressingMode::Impl => None,
        AddressingMode::Ind => Some(format!("(${:04X})", bus.read16(addr))),
        AddressingMode::XInd => Some(format!("(${:02X},x)", bus.read(addr))),
        AddressingMode::IndY => Some(format!("(${:02X}),y", bus.read(addr))),
        AddressingMode::Rel => Some(format!("${:02X}", bus.read(addr))),
        AddressingMode::Zpg => Some(format!("${:02X}", bus.read(addr))),
        AddressingMode::ZpgX => Some(format!("${:02X},x", bus.read(addr))),
        AddressingMode::ZpgY => Some(format!("${:02X},y", bus.read(addr))),
        _ => Some(addr_mode.name().to_owned()),
    }
}

fn operand_string_with_memory(
    bus: &Bus,
    addr: u16,
    cpu: &Cpu,
    inst_prop: &InstProp,
) -> Option<String> {
    match inst_prop.addr_mode {
        AddressingMode::A => Some("A".to_owned()),
        AddressingMode::Abs => {
            let op_addr = bus.read16(addr);
            if inst_prop.is_jump() {
                Some(format!("${:04X}", op_addr))
            } else {
                Some(format!("${:04X} = {:02X}", op_addr, bus.read(op_addr)))
            }
        }
        AddressingMode::AbsX => {
            let offset = bus.read16(addr);
            let op_addr = wrap_add16(offset, cpu.x as u16);
            Some(format!(
                "${:04X},X @ {:04X} = {:02X}",
                offset,
                op_addr,
                bus.read(op_addr)
            ))
        }
        AddressingMode::AbsY => {
            let offset = bus.read16(addr);
            let op_addr = wrap_add16(offset, cpu.y as u16);
            Some(format!(
                "${:04X},Y @ {:04X} = {:02X}",
                offset,
                op_addr,
                bus.read(op_addr)
            ))
        }
        AddressingMode::Imm => Some(format!("#${:02X}", bus.read(addr))),
        AddressingMode::Impl => None,
        AddressingMode::Ind => {
            let mid_addr = bus.read16(addr);
            let op_addr = bus.read16bug(mid_addr);
            Some(format!("(${:04X}) = {:04X}", mid_addr, op_addr))
        }
        AddressingMode::XInd => {
            let offset = bus.read(addr);
            let mid_addr = (offset as u16 + cpu.x as u16) & 0xff;
            let op_addr = bus.read16zero(mid_addr);
            Some(format!(
                "(${:02X},X) @ {:02X} = {:04X} = {:02X}",
                offset,
                mid_addr,
                op_addr,
                bus.read(op_addr)
            ))
        }
        AddressingMode::IndY => {
            let zaddr = bus.read(addr) as u16;
            let offset = bus.read16zero(zaddr);
            let op_addr = wrap_add16(offset, cpu.y as u16);
            // LDA ($89),Y = 0300 @ 0300 = 89
            Some(format!(
                "(${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                zaddr,
                offset,
                op_addr,
                bus.read(op_addr)
            ))
        }
        AddressingMode::Rel => {
            let next_addr = wrap_add16(addr, 1);
            let offset = bus.read(addr);
            let offseted_addr = offset_addr(next_addr, offset);
            Some(format!("${:04X}", offseted_addr))
        }
        AddressingMode::Zpg => {
            let op_addr = bus.read(addr);
            Some(format!(
                "${:02X} = {:02X}",
                op_addr,
                bus.read(op_addr as u16)
            ))
        }
        AddressingMode::ZpgX => {
            let zaddr = bus.read(addr) as u16;
            let op_addr = (zaddr + cpu.x as u16) & 0xff;
            Some(format!(
                "${:02X},X @ {:02X} = {:02X}",
                zaddr,
                op_addr,
                bus.read(op_addr)
            ))
        }
        AddressingMode::ZpgY => {
            let zaddr = bus.read(addr) as u16;
            let op_addr = (zaddr + cpu.y as u16) & 0xff;
            Some(format!(
                "${:02X},Y @ {:02X} = {:02X}",
                zaddr,
                op_addr,
                bus.read(op_addr)
            ))
        }
        _ => Some(inst_prop.addr_mode.name().to_owned()),
    }
}

fn cpu_status_string(cpu: &Cpu) -> String {
    format!(
        "A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU:{:3},{:3} CYC:{}",
        cpu.a,
        cpu.x,
        cpu.y,
        cpu.flags.value(),
        cpu.s,
        // https://wiki.nesdev.com/w/index.php/PPU_rendering#Line-by-line_timing
        // Each scanline lasts for 341 PPU clock cycles
        // 1 CPU cycle = 3 PPU cycles
        cpu.elapsed_cycles * 3 / 341,
        cpu.elapsed_cycles * 3 % 341,
        cpu.elapsed_cycles
    )
}

pub fn disasm_one_inst_and_status(bus: &Bus, addr: u16, cpu: &Cpu) -> usize {
    let byte = bus.read(addr as u16);
    let inst_prop = Cpu::inst_prop(byte);
    let operand_addr = (addr + 1) as u16;
    print!(
        "{:04X}  {:02X} {:5} ",
        addr,
        byte,
        operand_hex_string(bus, operand_addr, inst_prop.addr_mode)
    );
    let operand =
        operand_string_with_memory(bus, operand_addr, cpu, inst_prop).unwrap_or("".to_owned());
    let illegal = if inst_prop.illegal { "*" } else { " " };
    println!(
        "{illegal}{inst} {operand:27} {status}",
        illegal = illegal,
        inst = inst_prop.inst.name(),
        operand = operand,
        status = cpu_status_string(cpu)
    );
    1 + inst_prop.addr_mode.operand_len() as usize
}

pub fn disasm_one_inst<W: Write>(w: &mut W, bus: &Bus, addr: u16) -> usize {
    let byte = bus.read(addr as u16);
    let prop = Cpu::inst_prop(byte);
    let operand_addr = (addr + 1) as u16;
    write!(
        w,
        "{:04X}  {:02X} {:5}  ",
        addr,
        byte,
        operand_hex_string(bus, operand_addr, prop.addr_mode)
    )
    .unwrap();
    if let Some(operand) = operand_string(bus, operand_addr, prop.addr_mode) {
        writeln!(
            w,
            "{inst} {operand}",
            inst = prop.inst.name(),
            operand = operand
        )
        .unwrap();
    } else {
        writeln!(w, "{}", prop.inst.name()).unwrap();
    }
    1 + prop.addr_mode.operand_len() as usize
}

pub fn disasm<W: Write>(w: &mut W, bus: &Bus, addr: u16, size: usize) {
    let mut addr: u32 = addr as u32;
    let end: u32 = (addr as u32) + (size as u32);
    while addr < 0x10000 && addr < end {
        let size = disasm_one_inst(w, bus, addr as u16);
        addr += size as u32;
    }
}
