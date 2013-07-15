namespace FSharp.Enterprise

module Math = 

    module Interpolation =

        let inline linear value x y x' y' : ^T =
            y + (y' - y) * ((value - x)/(x' - x))

        let inline bilinear x y a b c d x1y1 x1y2 x2y1 x2y2 = 
            let r1 = (((x2y1 - x) / (x2y1 - x1y1)) * a) + (((x - x1y1)/(x2y1 - x1y1)) * b) 
            let r2 = (((x2y1 - x) / (x2y1 - x1y1)) * c) + (((x - x1y1)/(x2y1 - x1y1)) * d)
            let dy1 = (x2y2 - y) / (x2y2 - x1y2)
            let dy2 = (y - x1y2) / (x2y2 - x1y2)
            (dy1 * r1) + (dy2 * r2) 


    let inline intersection (x0:^T) (y0:^T) (x1:^T) (y1:^T) (x2:^T) (y2:^T) (x3:^T) (y3:^T) : (^T * ^T) option =
        // http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
        let zero = LanguagePrimitives.GenericZero
        let one = LanguagePrimitives.GenericOne
        let s1dx = x1 - x0
        let s1dy = y1 - y0
        let s2dx = x3 - x2
        let s2dy = y3 - y2
        let v = -s2dx * s1dy + s1dx * s2dy
        if v <> LanguagePrimitives.GenericZero then
            let s = (-s1dy * (x0 - x2) + s1dx * (y0 - y2)) / v
            let t = ( s2dx * (y0 - y2) - s2dy * (x0 - x2)) / v
            if s >= zero && s <= one && t >= zero && t <= one then
                let ix = x0 + (t * s1dx)
                let iy = y0 + (t * s1dy)
                Some (ix, iy)
            else
                None
        else
            None
