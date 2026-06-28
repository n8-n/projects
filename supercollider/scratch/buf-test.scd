s.quit;

(
~drums = Array.new;
// these samples are mono
~folder = PathName.new("/home/nathan/music_production/Samples/sitala_default_drums");

~folder.entries.do({ | path |
	~drums = ~drums.add(Buffer.read(s, path.fullPath));
});
)

(
SynthDef.new(\playBuf, {
	arg amp=0.7, out=0, buf, rate=1, t_trig=1, start=0, loop=0, da=2;
	var sig;

	sig = PlayBuf.ar(1, buf, rate, t_trig, start, loop, doneAction: da)!2;
	Out.ar(out, sig * amp);
}).add;

SynthDef.new(\bufRd, {
	arg amp=0.8, out=0, buf, rate=1, start=0, end;
	var sig, ptr;

	ptr = Phasor.ar(0, BufRateScale.kr(buf)*rate, start, end);
	// ptr = SinOsc.ar(rate, 3pi/2).range(start, end);
	sig = BufRd.ar(2, buf, ptr);
	sig = sig * amp;
	Out.ar(out, sig);
}).add;

)

~drums[0].play;


(
g = Group.new;

x = Synth.new(\playBuf, [\buf, ~drums[1], \rate, 0.01, \loop, 1], g);
y = Synth.new(\playBuf, [\buf, ~drums[0], \rate, 0.3, \loop, 1], g);
z = Synth.new(\playBuf, [\buf, ~drums[2], \rate, 0.2, \loop, 1], g);

)

g.freeAll;

~swell = Buffer.read(s, "/home/nathan/music_production/Samples/swell.wav");
~swell.query;
x = Synth.new(\bufRd, [\buf, ~swell, \start, 0, \end, 100000]);
x.set(\rate, 0.05);

y = Synth.new(\bufRd, [\buf, ~swell, \start, 0, \end, 80000, \rate, 0.8]);
y.set(\rate, 0.1);



(
~snare = Pdef(\snare,
	Pbind(
		\instrument, \playBuf,
		\buf, ~drums[10],
		\dur, Pseq([
			Pseq([1/8, 1/8, 1/4, 1/8], 4),
			Pgeom(1/2, 0.8, 30),
			Pn(Pseq([1/16]), 20),
			Pgeom(0.01, 1.1, 30)
		], inf),
		\stretch, 1,
		\amp, (Pkey(\dur) + 0.2).trace
	)
);


)

~snare.play;
~snare.stop;
















