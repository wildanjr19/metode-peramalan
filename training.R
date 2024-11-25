# metoper
# data musiman dengan regresi

library(readxl)
training <- read_excel("F:/Semester 3/Metode Peramalan/Project Data Musiman/Training_fix.xlsx")


regresi_ganda <- lm(LandAverageTemperature ~ Februari + Maret + April + 
                      Mei + Juni + Juli + Agustus + September + Oktober +
                      November + Desember, data = training)
# koefisien regresi
regresi_ganda

# tabel anava
summary(regresi_ganda)

# uji normalitas
shapiro.test(regresi_ganda$residuals)

# uji multikolinearitas
vif(regresi_ganda)

# uji autokorelasi
dwtest(regresi_ganda)

# uji heteroskedastisitas
bptest(regresi_ganda)

# evaluasi
y_topi <- predict(regresi_ganda, newdata = training)

merged_data <- data.frame(training$LandAverageTemperature, y_topi)

aktual <- training$LandAverageTemperature

# menghitung evaluasi
MSE <- mean((aktual - y_topi)^2)  
MAD <- mean(abs(aktual - y_topi))  
MAPE <- mean(abs((aktual - y_topi) / aktual)) * 100  
MPE <- mean((aktual - y_topi) / aktual) * 100

# Menampilkan hasil
cat("MSE:", MSE, "\n")
cat("MAD:", MAD, "\n")
cat("MAPE:", MAPE, "%\n")
cat("MPE:", MPE, "%\n")