MIDIClient.init;
MIDIIn.connectAll;

(
MIDIdef.noteOn(\noteOnTest, {
	arg vel, note, chan, src;
	//[vel, note].postln;
	~notes[note] = Synth.new(
		\tone,
		[
			\freq, note.midicps,
			\amp, vel.linexp(1,127, 0.05, 0.6),
			\gate, 1,
			\lfoRate, 0
		]
	);
});

MIDIdef.noteOff(\noteOffTest, {
	arg vel, note;
	//[vel, note].postln;
	~notes[note].set(\gate, 0);
	~notes[note] = nil;
});


MIDIdef.cc(\ccTest, {
	arg val, chan, src;
	//[val, chan, src].postln;
	~notes.do { arg synth; synth.set(\lfoRate, val.linlin(0,127,0,20)) };
}, ccNum:70);
)



(
SynthDef.new(\tone, {
	arg freq=440, amp=0.5, gate=0, lfoRate=0;
	var sig, env, lfo;
	lfo = { SinOsc.kr(lfoRate) };
	sig = LFTri.ar(freq!2, mul: lfo);
	env = EnvGen.kr(Env.adsr, gate, doneAction:2);
	sig = sig * env * amp;
	Out.ar(0, sig);
}).add;
)

~notes = Array.newClear(128);
