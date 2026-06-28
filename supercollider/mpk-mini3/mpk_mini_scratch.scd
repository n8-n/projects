

MIDIIn.connectAll;
s.newBusAllocators;


MIDIFunc.trace(false);



// BANKS
(
~bank = 16;

MIDIdef.cc(\pads, {
	|val, num, chan, src|

	// Filter MIDI off values, i.e. return from function.
	if (val == 0) { ^val };

	if (~debugLogs, {
		format("%", [val, num, chan, src]).postln;
	});
	~bank = num;
}, ccNum: (16..23), chan: 9);


)




// KNOBS
(
~debugLogs = true;

~ccList = (70..76);
~scaleList = [50] ++ 1.dup(~ccList.size - 1);
~defaultVals = 0.dup(~ccList.size);

~knobs = Dictionary();
~ccList.do({ |cc, i|
	var params = ();
	params[\scale] = ~scaleList[i];
	params[\bus] = Bus.control(s, 1).set(~defaultVals[i]);
	~knobs.put(cc, params);
});

~inc_dec = { |val, cc|
	var bus = ~knobs[cc][\bus];
	var add = ~knobs[cc][\scale];

	var busAdd = { |n|
		var curr = bus.getSynchronous();
		var sum = curr + n;

		if (~debugLogs, {
			postf("bank: %, bus: %, val: %\n", ~bank, cc, sum);
		});

		bus.setSynchronous(sum);
	};

	if(val == 1,
		{ busAdd.(add) },
		{ busAdd.(add.neg) }
	);
};


MIDIdef.cc(\knobs, { |val, num, chan, src|
	~inc_dec.(val, num);
	//postf("%\n", [val, num, chan, src]);
}, ccNum: ~ccList, chan: 0);

)


MIDIdef.all;




(
p = {
	var in = ~knobs[70][\bus].kr;
	in.wrap(-100, 100).poll(20) ;
}.play;
)


p = { b.kr.poll(20) }.play;

(
p.free;
b.set(0);
)


MIDIIn.connectAll;



// Install new class files here
Platform.userExtensionDir;


a = MpkMini3.new;
a.debugLogs_ = true;
a.setKnobScale(16, 71, 100);
b = a.getBus(16, 70, false);

w = {b.kr.poll(2)}.play;

b.set(0);

s.plotTree;
s.volume.gui;

v = [1, 2, 5, 6, 7];

v[0:6];

v[v.size - 1]

j = true;

if (j == false) { postln("hello") };

(
MIDIdef.cc(\volume, { |val, num, chan, src|

	var currentVolume = s.volume.volume;
	var toAdd = 0.1;

	var volumeAdd = { |n|
		{ s.volume.volume = currentVolume + n }.defer;
	};

	if(val == 1,
		{ volumeAdd.(toAdd) },
		{ volumeAdd.(toAdd.neg) }
	);
}, ccNum: 77);

)

s.volume.gui;

d = Dictionary();
d[16] = 122



(
SynthDef(\test, {
	arg freqBus, mixBus;
	var sig = SinOsc.ar(In.kr(freqBus));
	sig = FreeVerb.ar(sig, In.kr(mixBus).wrap(0, 1));
	Out.ar(0, sig!2);
}).add;
)


a.setKnobScale(16, 71, 0.01);
x = Synth(\test, [\freqBus, a.getBus(16, 70),\mixBus, a.getBus(16, 71)]);

x.free;


s.plotTree;






