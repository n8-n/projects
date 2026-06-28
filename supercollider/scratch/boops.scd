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


Pbindef(\melody).stop;
Pbindf










