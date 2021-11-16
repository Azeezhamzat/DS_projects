import json
from difflib import get_close_matches

data = json.load(open("data.json"))

def translate(word):
    word = word.lower() or word.upper()
    if word in data:
        return data[word]
    elif word.title() in data:
        return data[word.title()]
    elif len(get_close_matches(word , data.keys())) > 0 :
        print("did you mean %s instead? " %get_close_matches(word, data.keys())[0])
        decide = input("press y for yes or n for no ")
        if decide == "y" or "Y":
            return data[get_close_matches(word , data.keys())[0]]
        elif decide == "n" or "N":
            return("no match found ")
        else:
            return("You have entered wrong input please enter just y or n")
    else:
        print("pugger your paw steps on wrong keys")



word = input("Enter the word you want to search: ")
output = translate(word)
if type(output) == list:
    for item in output:
        print(item)
else:
    print(output)

#print(data.keys())