#version 300 es
precision highp float;
const float PI = 3.14159265359;
const highp float HEIGHT_SCALE = (1.0 / 257.0) * 9500.0;
highp float decode_height(highp vec2 c) {
  return (c.r + c.g * 256.0) * HEIGHT_SCALE - 500.0;
}

// The brightest channel that [tone_map] sends to half of full output, raised to
// the same 1.25 power as its argument. Lower it to brighten and harden the
// picture, raise it to soften; it is the only knob on the curve, an exposure in
// front of the curve being the same knob a second time.
const float TONE_PIVOT_125 = 0.278855; // 0.36^1.25

// Sunlight falling on the screen adds a veil to everything under it, and a veil
// takes contrast the picture never had to spare: a band of ridges used to span
// a fifth of the display range, and outdoors that fifth is what disappears.
// This is the sigmoid c^1.25 / (c^1.25 + pivot^1.25) -- a toe and a shoulder
// about a pivot, so the middle of the range, where the ground lies, is steeper
// on the way out. It approaches 1 without reaching it, which is what lets the
// direct sun be as strong as it is outside: snow and the sun's own disc keep
// their shape instead of flattening against white.
//
// The exponent is what the toe is made of, and the toe is the dangerous end of
// this curve. A slope in shade is lit by the sky alone, which puts it exactly
// where the toe bites: at 1.5 the north wall above the Plateau des Emparis came
// out at 0.135 where it had been at 0.193, a third of its brightness gone, and
// nothing was wrong with the contrast it had left -- there was simply no longer
// enough light under it to see that contrast by. At 1.25 the same wall comes
// out above where it started, keeping close to twice the local contrast it had
// before this curve existed. Going all the way to 1 overshoots: the wall passes
// 0.23, brighter than the day, and a rock face with some sun on it loses spread
// it used to have. Sizing the toe wants a view with a wall in full shade -- the
// midday panoramas used for everything else here have nothing in them the sky
// lights on its own, and they will call any of these exponents an improvement.
//
// The curve is driven by the brightest channel and the other two follow it,
// which is not the same as running it on each channel and matters for exactly
// the same wall. Per channel, the toe bites hardest on whichever channel is
// darkest: that wall's red fell from 0.118 to 0.096 while its blue rose past
// where it started, so it went from grey-blue to navy and still read as a hole
// at matched brightness. Scaling by the brightest channel leaves hue and
// saturation where the lighting put them, cannot push any channel past 1 -- the
// brightest maps into range by construction and the rest are below it -- and
// costs one curve instead of three. The sky pays a little for it, coming out a
// few percent below where it was, its red and green no longer bleached upwards.
//
// One thing not to try: the lighting does not lift those walls, so do not go
// looking for the answer in AMBIENT_LIGHT. Past a few kilometres most of what
// the eye lands on for a surface that dark is airlight -- around three quarters
// of it at 5 km -- and that leaves this curve as what decides how they read.
//
// Every shader drawing something the eye reads as part of the view has to apply
// this, and identically: the terrain fades into a horizon the sky shader draws
// from the same colour, and a curve on one and not the other would put a step
// along it.
vec3 tone_map(vec3 c) {
  vec3 s = max(c, 0.0);
  float m = max(max(s.r, s.g), s.b);
  float t = m * sqrt(sqrt(m)); // m^1.25, cheaper than a pow
  return s * (t / ((t + TONE_PIVOT_125) * max(m, 1e-6)));
}
