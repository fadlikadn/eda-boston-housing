English
CRIM - per capita crime rate by town
ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
INDUS - proportion of non-retail business acres per town.
CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
NOX - nitric oxides concentration (parts per 10 million)
RM - average number of rooms per dwelling
AGE - proportion of owner-occupied units built prior to 1940 
DIS - weighted distances to five Boston employment centres
RAD - index of accessibility to radial highways
TAX - full-value property-tax rate per $10,000
PTRATIO - pupil-teacher ratio by town
B-1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town 
LSTAT - % lower status of the population
MEDV - Median value of owner-occupied homes in $1000's


Bahasa
# Y	MedV	Harga median rumah yang ada di setiap kawasan di Kota Boston (MedV) (pada $1000's)
# X1	CRIM	Tingkat kejatahan (CRIM)
# X2	ZN	Persentase luas bangunan dengan lahan yang lebih dari 25000 Sq. Feet (ZN)
# X3	INDUS	Persentase luas lahan yang digunakan untuk bisnis non-retail (INDUS)
# X4	CHAS	Bangunan dekat sungai Charles (1), lainnya (0), (CHAS)
# X5	NOX	Konsentrasi oksida-oksida nitrogen (NOX)
# X6	RM	Rata-rata kamar yang ada di lingkungan tersebut (RM)
# X7	AGE	Umur bangunan (AGE)
# X8	DIS	Jarak rumah menuju tempat kerja / pusat pekerjaan
# X9	RAD	Banyaknya Akses menuju Jalan Raya (RAD)
# X10	TAX	Tarif Pajak per $10.000
# X11	PTRATIO	Rasio Murid dan Guru
# X12	B	1000 (Bk - 0.63)^2 di mana Bk adalah proporsi kulit hitam
# X13	LSTAT	Persentase pemilik rumah yang dianggap kelas bawah


EDA:
1. Bagaimana hubungan antara rata-rata jumlah kamar pada suatu rumah dengan besarnya tarif pajak? (RM & TAX)


2. Bagaimana hubungan antara jarak rumah menuju tempat kerja/pusat pekerjaan pada suatu rumah dengan besarnya tarif pajak? (DIS & TAX)


3. Bagaimana hubungan antara tingkat kriminalitas dengan persentase pemilik rumah yang dianggap kelas bawah (CRIM & LSTAT)


