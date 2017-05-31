set terminal svg size 1200, 500
set datafile separator ","
set xrange [0:6]
set key off

map(x) = (x <= 0.9      ? x/0.9 : \
         (x <= 0.99     ? ((x-0.9)/0.09) + 1 : \
         (x <= 0.999    ? ((x-0.99)/0.009) + 2 : \
         (x <= 0.9999   ? ((x-0.999)/0.0009) + 3 : \
         (x <= 0.99999  ? ((x-0.9999)/0.00009) + 4 : \
         (x <= 0.999999 ? ((x-0.99999)/0.000009) + 5 : 6))))))

set xtics ("0" 0, "90" 1, "99" 2, "99.9" 3, "99.99" 4, "99.999" 5, "99.9999" 6)
set xlabel "Percentiles"
set ylabel "Response time (us)"

plot filename using (map($2)):1 with lines
