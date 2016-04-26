# vb-math-function
Mathematical function abstraction featuring symbolic differentiation, implemented using recursion.

Warning: API and code is in Italian.

# What can it do?
Parse the string and store the abstraction in variable f
```
expression = "e^(sin(x))"
f = New Funzione(expression)
```

Checks f for validity
```
if Not f.Valida Then
	'handle the error somehow
```

Evaluates f in 3.14 and stores result in variable fa
```
a=3.14
fa=f.Calcola(a)

```

Computes derivative of f and stores it in new function object d: if d is valid evaluates it at 3.14 and stores the result in variable da
```
d = f.Derivata
if Not d.Valida
	"handle if f is not differentiable"
Else
	da=d.Calcola(a)
```

# Allowed expressions
Expressions make use of these symbols. Variable must always be called x. Decimal numbers are allowed.
```
x pi e
+ - * / ^
sqrt(arg)
cbrt(arg)
root(arg;index)
log(arg; base)
ln(arg)
abs(arg)
sin(arg)
cos(arg)
tan(arg)

```
# Motivation and possible improvements
This is a library I made in December 2014 in high school to allow my mathematics professor to integrate programming in his calculus lessons. It is not much more than a stub but I took great pride in it back then!

Since it was initially intended to be read by him and my classmates it is entirely implemented in Italian: examples, code, comments and API all need to be translated to English. I will find the time do this if somebody asks me (please do!).

This code has not been maintained since it was first written, compatibility bugs may have developed but I doubt it since it only uses the standard Math library for computing trig functions.

Also I don't know how a Visual Studio / Visual Basic library should be packaged for distribution so this also might need fixing.

I am sure that there are some performance improvements that can be easily made as well.

An important improvement would be to make the result of differentiation easy to read.
