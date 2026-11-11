s.boot;
s.quit;

(
s.newBusAllocators;
~lfoBus = Bus.control(s, 1);
~emptyLfoBus = Bus.control(s, 1);

t = TempoClock(120/60);

~lfo = SynthDef(\lfo, {
	arg freq=2, amp=20;
	var sig = SinOsc.ar(freq, mul: amp);
	Out.kr(~lfoBus, sig);
}).play;

SynthDef(\boop, {
	arg freq=200, atk=0.01, rel=1, filtFreq=600, filterLfoBus;
	var sig, filterFreq, env;
	env = Env.perc(atk, rel).kr(doneAction: 2);
	sig = Saw.ar(freq) * env;
	filterFreq = filtFreq + In.kr(filterLfoBus, 1);
	sig = MoogFF.ar(sig, filterFreq);
	Out.ar(0, sig!2);
}).add;

)


s.plotTree;


~lfo.set(\freq, 10);
~lfo.set(\amp, 50);

Synth(\boop, [\filterLfoBus, ~lfoBus, \rel, 4, \filtFreq, 600]);


(


Pbindef(\melody,
	\instrument, \boop,
	\filterLfoBus, ~lfoBus,
	\rel, 0.4,
	\scale, Scale.dorian,
	\root, -10,
	\mtranspose, Pseq([
		Pseq([4, 0, 2], 2),
		Pseq([4, 2, 0, 0, 2], 2)
	], inf),
	\dur, 0.5
).play(t);

)




/// ------------------------------------
/// Using Select Ugen to switch LFO on/off

(
SynthDef(\lfotest, {
	arg freq=440, atk=0.02, rel=0.7, amp=0.3, lfoOn=0;
	var env, sig, lfo;
	lfo = Select.kr(lfoOn, [DC.kr(0), SinOsc.kr(20).bipolar(200)]);
	env = Env.perc(atk, rel).kr(doneAction: Done.freeSelf);
	sig = VarSaw.ar(freq) * env;
	sig = BPF.ar(sig, 600 + lfo);
	sig = sig * amp;
	Out.ar(0, sig!2);
}).add;

)



x = Synth(\lfotest, [freq: rrand(200, 400), atk:4, rel:6]);
x.set(\lfoOn, 1);
x.set(\lfoOn, 0);










