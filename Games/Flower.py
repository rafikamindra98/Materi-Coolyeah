import turtle

t=turtle.Turtle()
s=turtle.Screen()

s.bgcolor('#262626')
t.pencolor('magenta')
t.speed(100)

col = ('cyan', 'blue', 'purple', 'violet', 'pink', 'red', 'orange', 'yellow', 'green', 'lime')

for n in range (10):
    for x in range (8):
        t.speed(x+10)
        for i in range(2):
            t.pensize(2)
            t.circle(8+n*20, 90)
            t.lt(90)
        t.rt(45)
    t.color(col[n%4])
s.exitonclick()
turtle.done()