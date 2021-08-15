use sdl2::event::Event;
use sdl2::image::{InitFlag, LoadTexture};
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;

mod bus;
mod cartridge;
mod cpu;
mod debug_font;
mod mapper;

use bus::Bus;
use cpu::AddressingMode;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn operand_hex_string(bus: &Bus, addr: u16, addr_mode: AddressingMode) -> String {
    match addr_mode.operand_len() {
        0 => "".to_owned(),
        1 => format!("{:02X}", bus.read(addr)),
        2 => format!(
            "{:02x} {:02X}",
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

fn disasm_one_inst(bus: &Bus, addr: u16) -> usize {
    let byte = bus.read(addr as u16);
    let prop = cpu::Cpu::inst_prop(byte);
    let operand_addr = (addr + 1) as u16;
    print!(
        "{:04X}  {:02X} {:5}  ",
        addr,
        byte,
        operand_hex_string(bus, operand_addr, prop.addr_mode)
    );
    if let Some(operand) = operand_string(bus, operand_addr, prop.addr_mode) {
        println!(
            "{inst} {operand}",
            inst = prop.inst.name(),
            operand = operand
        );
    } else {
        println!("{}", prop.inst.name());
    }
    1 + prop.addr_mode.operand_len() as usize
}

fn disasm(bus: &Bus, addr: u16, size: usize) {
    let mut addr: u32 = addr as u32;
    let end: u32 = (addr as u32) + (size as u32);
    while addr < 0x10000 && addr < end {
        let size = disasm_one_inst(bus, addr as u16);
        addr += size as u32;
    }
}
fn cpu_test() -> Result<()> {
    let cart = cartridge::read(std::path::Path::new("./nestest.nes"))?;
    println!(
        "cartridge: mapper={} prg_len={} chr_len={}",
        cart.mapper,
        cart.prg.len(),
        cart.chr.len()
    );
    let mapper = mapper::Mapper0::new(cart);
    let mut bus = bus::Bus::new(Box::new(mapper));
    // let vectors: [u16; 3] = [0xfffa, 0xfffc, 0xfffe];
    // for &addr in vectors.iter() {
    //     let data = mapper.read16(&cart, addr);
    //     println!("{:#x}: {:#x}", addr, data);
    // }
    let mut cpu = cpu::Cpu::new();
    cpu.reset(&bus);
    println!("PC={:#x}", cpu.pc);
    println!("-----");
    disasm(&bus, 0xc000, 0x3ffa);
    println!("-----");
    cpu.pc = 0xc000;
    for i in 0..10 {
        cpu.clock(&mut bus);
    }
    Ok(())
}

pub fn main() -> Result<()> {
    cpu_test()?;
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;
    let _image_context = sdl2::image::init(InitFlag::PNG | InitFlag::JPG)?;

    let window = video_subsystem
        .window("rust-sdl2 demo: Video", 800, 600)
        .position_centered()
        .opengl()
        .build()
        .map_err(|e| e.to_string())?;

    let mut canvas = window
        .into_canvas()
        .present_vsync()
        .build()
        .map_err(|e| e.to_string())?;
    let texture_creator = canvas.texture_creator();

    let mut texture = texture_creator
        .create_texture_streaming(PixelFormatEnum::RGB24, 256, 256)
        .map_err(|e| e.to_string())?;
    let font_texture = texture_creator.load_texture("images/charmap-oldschool_white.png")?;
    let debug_font = debug_font::DebugFont::new(font_texture);

    // Create a red-green gradient
    texture.with_lock(None, |buffer: &mut [u8], pitch: usize| {
        for y in 0..256 {
            for x in 0..256 {
                let offset = y * pitch + x * 3;
                buffer[offset] = x as u8;
                buffer[offset + 1] = y as u8;
                buffer[offset + 2] = 0;
            }
        }
    })?;

    let mut event_pump = sdl_context.event_pump()?;
    let mut angle: f64 = 0.0;

    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                _ => {}
            }
        }

        angle += 1.0;
        while angle > 360.0 {
            angle -= 360.0;
        }

        canvas.clear();
        canvas.copy(&texture, None, Some(Rect::new(100, 100, 256, 256)))?;
        canvas.copy_ex(
            &texture,
            None,
            Some(Rect::new(450, 100, 256, 256)),
            angle,
            None,
            false,
            false,
        )?;
        // canvas.copy(&font_texture, None, Some(Rect::new(300, 100, 128, 64)))?;
        // debug_font.copy(&mut canvas, Rect::new(300, 100, 128, 64))?;
        debug_font.render(&mut canvas, 100, 400, 3, "Hello, World!")?;
        canvas.present();
    }

    Ok(())
}
