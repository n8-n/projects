(
~ampBus = Bus.audio(s, 2);
)

(
SynthDef(\saw, {
	arg out, freq=300, rate=2;
	var sig, env;

	// env = LFTri.ar(rate).range(0, 0.6);
	env = EnvGen.ar(Env.perc(0.1, 0.5), levelScale: 0.7);
	sig = VarSaw.ar(freq) * env;
	Out.ar(out, sig!2);
}).add;
)

(
SynthDef(\amp, {
	arg in, rate=0.1;
	var sig = In.ar(in, 2);
	var env = SinOsc.ar(rate).unipolar;
	sig = sig * env;
	Out.ar(0, sig);
}).add;
)

(
a = Synth(\amp, [\in, ~ampBus]);

Pbind(
	\instrument, \saw,
	\out, ~ampBus,
	\freq, Pseq([50, 53, 57].midicps, inf),
	\dur, Pseq([1/4, 1/8, 1/8, 1/8], inf),
	\stretch, 2
).play;

)


a.set(\rate, 1);