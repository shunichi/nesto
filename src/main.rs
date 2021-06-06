use sdl2::event::Event;
use sdl2::image::{InitFlag, LoadTexture};
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;

mod cartridge;
mod debug_font;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub fn main() -> Result<()> {
    let cart = cartridge::read(std::path::Path::new("./nestest.nes"))?;
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