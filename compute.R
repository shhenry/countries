# my fun script

x = 0
y = 6
x1 = 100
n = .5

yp = x + y

h = (x1 - x)/n

for i=1:n
xhalf = x + .5*h
xnew = x + h
k1 = f(x,y) #
u1 = y + .5*h*k1
k2 = f(xhalf,u1)
u2 = y + .5*h*k2
k3 = f(xhalf,u2)
u3 = y + h*k3
k4 = f(xnew, u3)
k = (k1 + 2*k2 + 2*k3 + k4)/6
x = xnew
y = y + k*h
end

  
