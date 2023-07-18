SinOsc s => Gain g => dac;

220 => s.freq;
0.5 => g.gain;

while ( true ) {
	5::ms => now;	
}
