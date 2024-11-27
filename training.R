# training

library(readxl)
training <- read_excel("F:/Semester 3/Metode Peramalan/Project Data Musiman/Training_fix.xlsx")

# model
regresi_ganda <- lm(LandAverageTemperature ~ t + Februari + Maret + April + 
                      Mei + Juni + Juli + Agustus + September + Oktober +
                      November + Desember, data = training)

# ringkasan model
summary(regresi_ganda)

# koefisien regresi
regresi_ganda

# tabel anava
anova(regresi_ganda)

# uji normalitas
shapiro.test(regresi_ganda$residuals)

# uji multikolinearitas
vif(regresi_ganda)

# uji autokorelasi
dwtest(regresi_ganda)

# uji heteroskedastisitas
bptest(regresi_ganda)

# evaluasi
# nilai prediksi
y_topi_training <- predict(regresi_ganda, newdata = training)

# nilai aktual
aktual_training <- training$LandAverageTemperature

# menghitung evaluasi
MSE <- mean((aktual_training - y_topi_training)^2)
RMSE <- sqrt(MSE)
MAD <- mean(abs(aktual_training - y_topi_training))  
MAPE <- mean(abs((aktual_training - y_topi_training) / aktual_training)) * 100  
MPE <- mean((aktual_training - y_topi_training) / aktual_training) * 100

# Menampilkan hasil
cat("MSE:", MSE, "\n")
cat("RMSE:", RMSE, "\n")
cat("MAD:", MAD, "\n")
cat("MAPE:", MAPE, "%\n")
cat("MPE:", MPE, "%\n")