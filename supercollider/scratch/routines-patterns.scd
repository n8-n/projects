


// Setup
(
t = TempoClock(110/60).permanent_(true);

~playTone = {|freq|
	{SinOsc.ar([freq+2, freq-2]) *
		Line.kr((0.5 * ~note_type/2), 0, {t.tempo / 2},  doneAction: 2)
	}.play;
};

SynthDef(\ping, {
	arg freq=200, noteType, tempo;
	var sig, env, maxVolume, attack;
	maxVolume = (0.1 + noteType/2).min(0.3);
	attack = 0.01 + noteType/5;
	env = EnvGen.ar(Env.perc(attack, tempo / 1.5).range(0, maxVolume), doneAction: 2);
	sig = Saw.ar([freq+2, freq-2]) * env;
	Out.ar(0, sig);
}).add;
)


// Changing variables
(
~notes = Pbrown(60, 85, 4, inf).asStream;
~tempo = Pbrown(100, 150, 20, inf).asStream;

~num_notes = 6;
~note_type = 2;
~wait_type = 1/2;
)

s.plotTree;


// Routines
(
r = Routine({
	loop({
		~notes.nextN(~num_notes).do({ |n|
			Synth(\ping, [\freq, n.midicps, \tempo, t.tempo, \noteType, ~note_type]);
			//~playTone.(n.midicps);
			~note_type.yield;
		});
		~wait_type.yield;
	});
});


~r2 = Routine({
	loop({
		var this_tempo = ~tempo.next()/60;
		t.tempo = this_tempo;
		(this_tempo * 4).yield;
	});
});


~both = Routine({
	r.play(t);
	~r2.play(t);
}).play;
)


t.tempo * 60;





