#Kort intro til R
#Thomas Vils Pedersen, Matematik og modeller
#Neden for er stillet Spørgsmål A-G 
#De tilhørende svar viser typiske måder at bruge R på
#Opgaverne R1-R23 giver mulighed for at træne disse ting mere selvstændigt 

#Spørgsmål A: hvad er kvadratroden af 2+2?
sqrt(2+2)
resultat <- sqrt(2+2)
resultat

#Spørgsmål B: Hvordan tegnes grafer og hvordan bestemmes skæringspunkter? 
f <- function(x){10*exp(-x)}
f(0)
f(1)
f(0:5)
g <- function(x){10/(1+exp(-x))}
plot(f,0,5)
plot(g,0,5)
plot(f,0,5)
plot(g,0,5,add=TRUE)
h <- function(x){f(x)-g(x)}
krydspunkt <- uniroot(h,c(0,5))
krydspunkt
points(krydspunkt$root, f(krydspunkt$root), pch=19, col="red")

#Spørgsmål C: Hvordan regnes og fremskrives med matricer?
A<-matrix(c(0.8,0.2,0,0,0,0.7,0.1,0.2,0,0,1.0,0,0,0,0,1.0),4)
A
A[,2]
x0 <- c(100,0,0,0)
x1 <- A%*%x0
x1
x2 <- A%*%x1
x2
Ainv <- solve(A)
Ainv
y <- Ainv%*%c(50,30,5,15)
y
y <- Ainv%*%y
y

x0 <- c(50,50,0,0)
x <- x0
for (k in (1:20)) {x <- A%*%x;}
x

#Spørgsmål D: Hvordan indlæses data i R?
#Svar: Opgave R23
 
#Spørgsmål E: Hvordan bestemmes egenværdier og -vektorer?
A<-matrix(c(1,4,4,1),2)
A
eigen(A)
lambda <- eigen(A)$value
lambda
lambda[2]
Q <- eigen(A)$vectors
Q
Q[,2]
A%*%Q[,2]-lambda[2]*Q[,2]

# Komplekse egenværdier og egenvektorer
A<-matrix(c(1,4,-4,1),2)
A
eigen(A)
lambda <- eigen(A)$value
lambda
lambda[2]
Q <- eigen(A)$vectors
Q
Q[,2]
A%*%Q[,2]-lambda[2]*Q[,2]

#Spørgsmål F: Hvordan regnes med komplekse tal?
options(digits=3)
z<-3+4*i
# Det dur ikke
z<-3+4i
# Det dur (tak til Jacob Engelbrecht)
(3+4i)*(3-4i)
z
# Vi kan derefter regne med z som med reelle tal
8+3*z
4+z/(1+z)
z^3
(1+1i)^(-5)
# Bemærk betegnelserne Re(x), Im(x), Conj(x), Mod(x), Arg(x) for 
# realdel, imaginærdel, modulus, argument, konjugeret
z
Re(z)
Im(z)
Conj(z)
Mod(z) # abs(z) kan også bruges
Arg(z)
z*Conj(z)
# Hvad er argumentet for tallet -4+4i?
w<- -4+4i
w
atan(4/(-4))
Arg(w)


#Spørgsmål G: Hvordan laves gentagelser? (for-løkker)
#Hvad sker der i næste linje?
x<-0; for (k in(1:3)) {x<-c(k,x)}; x;

#Gentagelser (fremskrivning af kaninbestand)
M<-matrix(c(2.0,0.7,1.5,0.4),2)
x0<-c(100,0)
V<-matrix(x0,2)
x<-x0; for (k in (1:10)) {x<-M%*%x; V<-cbind(V,x)};
V
plot((0:10),V[1,])
points((0:10),V[2,])
points((0:10),V[2,], pch=19, col="red")
plot((0:10),V[2,]/V[1,])
plot((0:10),V[2,]/V[1,], xlab="år", ylab="antal unge delt med antal gamle", pch=8, col="purple", main="Forholdet mellem
unge og gamle hunkaniner")

