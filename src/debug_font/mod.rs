use sdl2::rect::Rect;
use sdl2::render::{Canvas, RenderTarget, Texture};

pub struct DebugFont<'a> {
    pub texture: Texture<'a>,
}

impl<'a> DebugFont<'a> {
    pub fn new(texture: Texture) -> DebugFont {
        DebugFont { texture: texture }
    }

    pub fn copy<T: RenderTarget>(&self, canvas: &mut Canvas<T>, rect: Rect) -> Result<(), String> {
        canvas.copy(&self.texture, None, Some(rect))
    }

    pub fn render<T: RenderTarget>(
        &self,
        canvas: &mut Canvas<T>,
        x: i32,
        y: i32,
        scale: u32,
        text: &str,
    ) -> Result<(), String> {
        let tw = 7;
        let th = 9;
        let mut dx = 0;
        for c in text.chars() {
            let code = c as i32;
            // println!("{:?}", code);
            if 32 < code && code < 127 {
                let tx = (code - 32) % 18 * 7;
                let ty = (code - 32) / 18 * 9;
                canvas.copy(
                    &self.texture,
                    Some(Rect::new(tx, ty, tw, th)),
                    Some(Rect::new(
                        x + dx * (scale as i32),
                        y,
                        tw * scale,
                        th * scale,
                    )),
                )?;
            }
            dx += tw as i32;
        }
        Ok(())
    }
}
