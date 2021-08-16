pub fn wrap_add16(a: u16, b: u16) -> u16 {
    (std::num::Wrapping(a) + std::num::Wrapping(b)).0
}

pub fn wrap_add8(a: u8, b: u8) -> u8 {
    (std::num::Wrapping(a) + std::num::Wrapping(b)).0
}

pub fn wrap_sub8(a: u8, b: u8) -> u8 {
    (std::num::Wrapping(a) - std::num::Wrapping(b)).0
}

pub fn offset_addr(addr: u16, offset: u8) -> u16 {
    if (offset & 0x80) == 0 {
        wrap_add16(addr, offset as u16)
    } else {
        wrap_add16(addr, 0xff00 | offset as u16)
    }
}

pub fn crossing_page_cycle(addr0: u16, addr1: u16) -> u8 {
    if (addr0 & 0xff00) != (addr1 & 0xff00) {
        1
    } else {
        0
    }
}
