from tkinter import *

root = Tk()

e = Entry(root, width=50, bg='light blue', fg='purple', borderwidth=10)
e.pack()
e.insert(0, "Enter your name ")
def myClick():
    hello = "Hello " + e.get()
    myLabel = Label(root, text= hello)
    myLabel.pack()

myButton = Button(root,text ="Enter your name", command=myClick, fg="yellow", bg="blue") 
#remember to not put the ()calling the function myClick

myButton.pack()
root.mainloop()