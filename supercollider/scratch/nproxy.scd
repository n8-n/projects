s.boot;

(
Ndef(\sine, {
	Splay.ar(SinOscFB.ar([
		LFTri.ar(1/9).range(195, 150),
		// XLine.ar(195, 150, 4.5),
		//XLine.ar(195, 250, 5)],
		LFTri.ar(1/10).range(195, ~knobs[70][\bus].kr)],
		feedback: SinOsc.kr(~knobs[71][\bus].kr + LFNoise1.kr(0.1, mul: 5)).range(0.02, 0.5), mul: 0.6),
	center: SinOsc.kr(0.03)
	)
})
.fadeTime_(0.5).play;

)


Ndef(\sine).stop(5);