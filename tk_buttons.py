from tkinter import *
#uncomment the lines to check them out
root = Tk()
#Creating a label widget
# myButton = Button(root,text ="Click me!")    #Button(root,text ="Click me!", state=DISABLED)
#setting state as 'DISABLED' will render the button unclickable

# myButton = Button(root,text ="Click me!", padx = 50, pady = 50)

def myClick():
    myLabel = Label(root, text="Look! I clicked a Button!!")
    myLabel.pack()

myButton = Button(root,text ="Click me!", command=myClick, fg="yellow", bg="blue") #remember to not put the ()calling the function myClick
myButton.pack()
root.mainloop()