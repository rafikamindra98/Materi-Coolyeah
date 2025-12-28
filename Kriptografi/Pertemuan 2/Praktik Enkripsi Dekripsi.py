#---CAESAR CIPHER---#
#---ENKRIPDSI---#
def enkrip(plainA,key):
    cipherA=''
    for i in range(len(plainA)):
        cipherA=cipherA+chr((ord(plainA[i])+key-97)%26+97)
    print('Cipherteks:',cipherA)

#---Utama---#
print('CAESAR CIPHER')
print('ENKRIPSI')
plainA=input('Plainteks: ')
key=int(input('key(angka): '))
enkrip(plainA,key)
input()

#---DEKRIPSI---#
def dekrip(plainB,key):
    dekripA=''
    for i in range(len(plainB)):
        dekripA=dekripA+chr((ord(plainB[i])-key-97)%26+97)
    print('Cipherteks:',dekripA)

#---Utama---#
print('CAESAR CIPHER')
print('DEKRIPSI')
plainB=input('Cipherteks: ')
key=int(input('key(angka): '))
dekrip(plainB,key)
input()