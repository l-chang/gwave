* hspice deck for simple RC low-pass filter

R1 in out 10k
C1 out GND 0.01u

Vin in GND 0.0 AC=1.0

.ac dec 1000 100 100k

.graph ac v(in) v(out)
.option POST=2 probe

.end

