s.quit;
s.plotTree;

(
SynthDef(\simple, {
	arg out=0, freq=400, gate=1,
	atk=0.5, dec=2, rel=20, pan=0;
	var sig, env;

	env = EnvGen.ar(Env.perc(atk, dec), gate, doneAction: Done.freeSelf);
	// freq = freq * env;
	sig = Resonz.ar(Saw.ar(freq), freq * 2) * env;
	sig = Pan2.ar(sig, pan);
	Out.ar(out, sig * 0.7) ;
}).add;

)


x = Synth(\simple);



(
~quarter1 = (60 / 100) * 4;
~quarter2 = (60 / 101) * 4;

~durs = [1/8];


~pattern = Pbind(
	\instrument, \simple,
	// \midinote, Pseq([50, 54, 57], inf),
	\scale, Scale.dorian,
	\root, -10,
	\mtranspose, Pseq([0, 2, 4, 7, 4], inf),
	\stretch, ~quarter1,
	\dur, Pn(Plazy { Pseq(~durs) }),
	// \dur, Pdefn(\durs, Pseq(~durs, inf)),
	\atk, Prand((0.1..0.4), inf),
	\rel, Pkey(\atk, inf) * 1.5,
	\pan, 0);
);


(
Pdef(\sq1, ~pattern);
x = Pbindef(\sq1,
	\pan, -0.7

).play(quant: 1);

Pdef(\sq2, ~pattern);
y = Pbindef(\sq2,
	\stretch, ~quarter2,
	\pan, 0.7
).play(quant: 1);

)

x.pause;
x.resume;

y.pause;
y.resume;



(
Pdef(\sq1, ~pattern);
x = Pbindef(\sq1,
	\pan, Pseq([
		Pseries(1, -0.1, 20),
		Pseries(-1, 0.1, 20),
	], inf),
	\atk, Prand((0.3..0.6), inf),
).quant_(~quarter1);

Pdef(\sq2, ~pattern);
y = Pbindef(\sq2,
	\stretch, ~quarter2,
	\pan, Pseq([
		Pseries(-1, 0.1, 20),
		Pseries(1, -0.1, 20),
	], inf),
	\root, -10
).quant_(~quarter2);

)



~durs = [1/8, 1/8, 1/16];
~durs = [1/4, 1/16, 1/8, 1/8, 1/4];

// Pdefn(\durs, Pseq([1/4, 1/16, 1/8, 1/8, 1/32, 1/32, 1/4], inf));






