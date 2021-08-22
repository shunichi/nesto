use sdl2::event::Event;
use sdl2::image::{InitFlag, LoadTexture};
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;
use sdl2::render::Texture;
use std::fs::File;
use std::io::{BufWriter, Write};

mod bus;
mod cartridge;
mod cpu;
mod debug_font;
mod mapper;
mod utils;

use cartridge::Cartridge;
use cpu::disasm::disasm;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn chr_pixel(buffer: &[u8], plane: usize, x: usize, y: usize) -> u8 {
    let offset = ((plane & 1) << 12) | (((y / 8) & 0xf) << 8) | (((x / 8) & 0xf) << 4);
    let fine_y = y % 8;
    let b0: u8 = buffer[offset + fine_y];
    let b1: u8 = buffer[offset + 8 + fine_y];
    let fine_x = x % 8;
    ((b0 >> (7 - fine_x)) & 1) | (((b1 >> (7 - fine_x)) & 1) << 1)
}

const GRAYS: [u8; 4] = [64, 128, 172, 0];
fn create_pattern_table_texture(texture: &mut Texture, cart: &Cartridge) -> Result<()> {
    println!("{:?}", &cart.chr[0..16]);
    texture.with_lock(None, |buffer: &mut [u8], pitch: usize| {
        for y in 0..128 {
            for x in 0..128 {
                let offset = y * pitch + x * 3;
                let pixel = chr_pixel(&cart.chr, 0, x, y);
                // print!("{},", pixel);
                let b = GRAYS[(pixel & 0x11) as usize];
                buffer[offset + 0] = b;
                buffer[offset + 1] = b;
                buffer[offset + 2] = b;
            }
            // println!("");
        }

        for y in 0..128 {
            for x in 0..128 {
                let offset = y * pitch + (x + 128) * 3;
                let pixel = chr_pixel(&cart.chr, 1, x, y);
                let b = GRAYS[(pixel & 0x11) as usize];
                buffer[offset + 0] = b;
                buffer[offset + 1] = b;
                buffer[offset + 2] = b;
            }
        }
    })?;
    Ok(())
}

fn rom_test(path: &str) -> Result<Cartridge> {
    println!("rom_test: {}", path);
    // let cart = cartridge::read(std::path::Path::new("./nestest.nes"))?;

    // sm
    // 8082 NMI
    // 8000 Reset
    // FFF0 IRQ/BRK
    // let cart = cartridge::read(std::path::Path::new("./sm.nes"))?;
    let cart = cartridge::read(std::path::Path::new(path))?;
    println!(
        "cartridge: mapper={} prg_len={} chr_len={}",
        cart.mapper,
        cart.prg.len(),
        cart.chr.len()
    );

    {
        let file = File::create("./disasm.txt")?;
        let mut out = BufWriter::new(file);
        let mapper = mapper::Mapper0::new(&cart);
        let bus = bus::Bus::new(Box::new(mapper));
        disasm(&mut out, &bus, 0x8000, cart.prg.len() - 12);
    }

    Ok(cart)
}

fn cpu_test() -> Result<()> {
    let cart = cartridge::read(std::path::Path::new("./nestest.nes"))?;
    println!(
        "cartridge: mapper={} prg_len={} chr_len={}",
        cart.mapper,
        cart.prg.len(),
        cart.chr.len()
    );
    let mapper = mapper::Mapper0::new(&cart);
    let mut bus = bus::Bus::new(Box::new(mapper));
    let mut cpu = cpu::Cpu::new();
    cpu.reset(&bus);
    cpu.pc = 0xc000;
    for _i in 0..26554 {
        cpu.clock(&mut bus);
    }
    Ok(())
}

pub fn main() -> Result<()> {
    let args: Vec<_> = std::env::args().collect();
    if args.len() < 2 {
        std::process::exit(1);
    }
    let cart = rom_test(&args[1])?;

    // cpu_test()?;
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
        .create_texture_streaming(PixelFormatEnum::RGB24, 256, 128)
        .map_err(|e| e.to_string())?;
    let font_texture = texture_creator.load_texture("images/charmap-oldschool_white.png")?;
    let debug_font = debug_font::DebugFont::new(font_texture);

    // Create a red-green gradient
    // texture.with_lock(None, |buffer: &mut [u8], pitch: usize| {
    //     for y in 0..256 {
    //         for x in 0..128 {
    //             let offset = y * pitch + x * 3;
    //             buffer[offset] = x as u8;
    //             buffer[offset + 1] = y as u8;
    //             buffer[offset + 2] = 0;
    //         }
    //     }
    // })?;

    create_pattern_table_texture(&mut texture, &cart)?;

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
        canvas.copy(&texture, None, Some(Rect::new(100, 100, 512, 256)))?;
        // canvas.copy_ex(
        //     &texture,
        //     None,
        //     Some(Rect::new(450, 100, 256, 128)),
        //     angle,
        //     None,
        //     false,
        //     false,
        // )?;
        // canvas.copy(&font_texture, None, Some(Rect::new(300, 100, 128, 64)))?;
        // debug_font.copy(&mut canvas, Rect::new(300, 100, 128, 64))?;
        debug_font.render(&mut canvas, 100, 400, 3, "Hello, World!")?;
        canvas.present();
    }

    Ok(())
}
