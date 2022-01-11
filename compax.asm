;*******************************************************************************
; NOMBRE       : COMPAX.ASM
; DESCRIPCION  : Comparardor de Amplificadores y Pantallas acust.
; LENGUAJE     : Ensamblador para el motorola m68705C8
; EDITOR       : EMETERIO
; OBSERVACIONES:
;*******************************************************************************

;*******************************************************************************
; Flag definitions
;
; flags      bit 0 ,si esta a '1' es que ya tiene no de aplif. entrado
;             "  1 ,se pone cuando han entrado la primera puls. de una opcion
;             "  2 ,se pone cuando se ha acabado de editar
;             "  3 ,se pone en comp. cuando va a comparar pantallas
;             "  4 ,lo quita en timer al llegar a cero contim
;             "  5 ,---|
;             "  6 ,---|---- se usan en rutinas del display (Oscar)
;             "  7 ,---|
;
; flag1     bit 0 ,lo pone cuando hay una puls. de teclado y lo resetea si no
;************************ definicion del bufer de transmision*****************

; [EC]-[A1]-[amplif]-[pantall]  4 bytes todo en binario

RAM                 def       $30

;*******************************************************************************
                    #RAM      RAM
;*******************************************************************************
                                                  ; (cada 64 es una página)
PgWidth             rmb       1
contim              rmb       1                   ; contador para tiempos del timer
savex               rmb       1                   ; guarda 'x' en tecla
savex1              rmb       1                   ; para operar punteros en "acep"
contbuf             rmb       1                   ; contador de las opciones entradas
contfle             rmb       1                   ; puntero de la flecha a sumar con PUNFLE
buftran             rmb       4                   ; buffer de transmision via RS-485
contop              rmb       1                   ; contador de opciones entradas

flags               rmb       1                   ; definido arriba
flag1               rmb       1

sava                rmb       1
svx                 rmb       1
tmp2                rmb       1

pag                 rmb       1                   ; Página actual.
ncars               rmb       1                   ; Número de bytes escritos en la página actual
tmp3                rmb       1

bufdis              rmb       84                  ; bufer del display

;*******************************************************************************
; PORT definitions
;*******************************************************************************

PORTA               equ       $00
PORTB               equ       $01
PORTC               equ       $02
PORTD               equ       $03
PCA                 equ       $04
PCB                 equ       $05
PCC                 equ       $06
PCD                 equ       $07

SCDAT               equ       $11                 ; Serial Comunications Data Register
SCCR1               equ       $0e                 ; Serial Comunication Register 1
SCCR2               equ       $0f                 ; Serial Comunication Register 2
SCSR                equ       $10                 ; Serial comunication Status Register
BAUD                equ       $000d               ; Baud Rate Register
TCR                 equ       $12                 ; Timer Control Register
TSR                 equ       $13                 ; Timer Status Register
SPCR                equ       $0a                 ; spcr (serial peripheral control register)
SPSR                equ       $0b                 ; Serial Peripheral Status Register
SPDR                equ       $0c                 ; Serial Peripheral Data I/O Register
TCRH                equ       $001a               ; timer count register (high)
TCRL                equ       $001b               ; timer count register (low)
ENTRY               equ       $100

E                   equ       7                   ; Bit del puerto donde está conectada la línea E
WR                  equ       5                   ; Bit del puerto donde está conectada la línea WR
DI                  equ       4                   ; Bit del puerto donde está conectada la línea RS
RST                 equ       6
CS                  equ       3

ST_RESET            pin       flags,7
ST_ONOFF            pin       flags,6
ST_BUSY             pin       flags,5

CTRL_DISP           equ       2                   ; PUERTO DONDE ESTÁN CONECTADAS LAS LÍNEAS DE CONTROL
DAT_DISP            equ       0                   ; PUERTO DONDE ESTÁN CONECTADAS LAS LÍNEAS DE DATOS

;*******************************************************************************
; CONSTANTS
;*******************************************************************************

PUNAMO              equ       !05                 ; puntero de princ. de mem. de amplif. (decenas)
PUNPAO              equ       !26                 ; "    " "    " "   " pantallas (decenas)
PUNFLE              equ       !47                 ; "    " la flecha de elementos en funcionamiento
PUNAME              equ       !71                 ; "    " donde pondra lo que entren de num. de amplif.
PUNPAE              equ       !83                 ; "    " donde pondra lo que entren de num. de pantallas

;*******************************************************************************
                    #ROM      $100
;*******************************************************************************

Start               proc
                    rsp                           ; reset stack pointer
                    lda       #$80
                    sta       $1fdf               ; carga el option register
                    clra
                    clrx
          ;-------------------------------------- ; zero RAM memory
Zero@@              clr       RAM,x
                    incx
                    cpx       #$ff
                    bne       Zero@@
          ;-------------------------------------- ; INICIALIZACION DE LOS PORTS
                    mov       #$f0,PCA            ; cuatro byts bajos ent. los altos entr. (teclado)
                    lda       #$ff
                    sta       PCC
                    sta       PCB
                    clr       PORTA
                    clr       PORTB
                    clr       PORTC
                    clr       PCD

                    clra
                    ldx       #$33                ; velocidad de transmision 1200 Bds.
                    jsr       SciInit             ; inicializa el port serie
          ;--------------------------------------
                    bset      4,PORTB             ; salida (RS-485) activada
                    bset      1,PORTC             ; 74ls244 desactivado

                    jsr       InitDisp            ; Inicializa el display
                    jsr       DispOn              ; Activa el display
                    jsr       Cls                 ; Borra el display
                    jsr       pretim              ; prepera el timer
                    bset      5,TCR

                    cli

                    clrx
carg                lda       texto1,x
                    beq       carg0
                    sta       bufdis,x            ; carga el texto en RAM para poder operar
                    incx
                    bra       carg

carg0               mov       #$EC,buftran
                    mov       #$A1,buftran+1      ; carga el frame en el buffer de transmision

                    clrx
res2                lda       texto,x
                    beq       res4
                    jsr       print               ; saca pantalla de inicio
                    incx
                    bra       res2

res4                mov       #20,contim
                    bset      4,flags
                    brset     4,flags,*
                    sei
                    clrx

                    jsr       refdis              ; saca a display el formato de funcionamiento
                    bra       main

borr                jmp       BORRA               ; tira un espacio atras y borra lo que hay
dreta               jmp       dret
esquer              jmp       esq
main1               bclr      flag1
main                jsr       tecla
                    bcs       main1
                    brset     flag1,main          ; si esta es que aun es la misma pulsacion
                    bset      flag1               ; hay una puls. y no coje ninguna mas si hay flag
                    cmpa      #'A'
                    beq       dreta               ; quiere escojer una opcion de la derecha
                    cmpa      #'B'
                    beq       esquer              ; "       " "    " "  " izquierda
                    cmpa      #'C'
                    beq       borr
                    cmpa      #$0d
                    beq       acepta              ; return
                    cmpa      #$3a
                    bpl       main
                    brset     2,flags,main        ; si esta el bufer lleno, no se puede ent. mas
                    sta       sava                ; guarda el num. entrado
                    brset     flags,pant          ; si esta puesto, ya hay amplif. entrado
                    ldx       #PUNAME             ; punto de entrada de edicion del ampli.
                    brset     1,flags,desp        ; esta es la segunda cifra de una opcion
                    sta       bufdis,x
main0
                    bset      1,flags             ; falta la segunda cifra
                    jsr       refdis              ; saca todo al display
                    bra       main

desp                lda       bufdis,x            ; coje la primera cifra para correrla (decenas)
                    decx
                    sta       bufdis,x
                    incx
                    lda       sava                ; recupera el num. que han entrado
desp0               sta       bufdis,x
                    jsr       refdis
                    bclr      1,flags             ; la segunda pluls. ya ha llegado
                    brclr     flags,desp1
                    bset      2,flags             ; la edicion esta cmpleta, solo return o borr.
                    bclr      flags
                    bra       main

desp1               bset      flags               ; ahora falta que entren pantallas
                    bra       main

pant                ldx       #PUNPAE
                    brset     1,flags,desp        ; esta es la segunda cifra de una opcion
                    sta       bufdis,x
                    bra       main0

;*******************************************************************************

acp2                jmp       acep2
acp3                jmp       acep3

;*******************************************************************************
; han pulsado return, y lo que hay en la linea de edicion
; lo va a pasar a las lineas superores y ejecuar la opcion

acepta              proc
                    brset     2,flags,acep1       ; si la edicion esta completa, lo emitira
                    brset     1,flags,acp2        ; solo hay una cifra, pondra cero en decenas
                    brset     flags,acp3          ; hay ampif. entrado, pondr‚ las mismas pant.
                    jmp       acep5               ; no hay nada, pondra el amp. que esta sonando

                    lda       contbuf
                    beq       main

acep1               lda       contop
                    bne       ace                 ; ¨es la primera opcion?
                    lda       #PUNAMO
                    sta       savex
                    lda       #PUNPAO
                    sta       savex1
                    jmp       acep4

ace                 mov       #PUNAMO,savex       ; va a comparar si ya existe esta opcion
                    mov       #PUNAME,savex1
                    bclr      3,flags             ; este se pondra cuando ya haya visto los ampli.
                    clr       sava                ; contador de opciones comprobadas

comp                ldx       savex
                    lda       bufdis,x            ; coje la primera opc. para comp.
                    ldx       savex1
                    cmpa      bufdis,x            ; compara las unidades
                    bne       noig
                    ldx       savex
                    decx
                    lda       bufdis,x            ; compara las decenas
                    ldx       savex1
                    decx
                    cmpa      bufdis,x
                    beq       siigu

noig                bclr      3,flags
                    lda       sava
                    cmpa      contop              ; mira si ya ha llegado al final
                    beq       fin1
                    add       #3
                    sta       sava
                    lda       #PUNAMO
                    add       sava
                    sta       savex
                    lda       #PUNAME
                    sta       savex1
                    bra       comp

;                   lda       savex
;                   add       #3
;                   sta       savex
;                   bra       comp

siigu
; ha encntrado el amplif. igual, va a mirar los altav.

                    brset     3,flags,fin         ; si es '1' es que hay una opcion igual
                    lda       sava
                    bne       siigu1
                    lda       #PUNPAO
siigu2
                    sta       savex
                    lda       #PUNPAE             ; va a comp. las pantall. de la misma opcion
                    sta       savex1
                    bset      3,flags
                    bra       comp

siigu1              lda       #PUNPAO
                    add       sava
                    bra       siigu2

fin
; hay una opcion igual que la entrada £ltima
; no la pongo en el buffer, la voy a ejecutar

                    lda       #'-'
                    ldx       #PUNAME
                    sta       bufdis,x            ; pone los '-' en los puntos de entrada
                    decx
                    sta       bufdis,x
                    ldx       #PUNPAE
                    sta       bufdis,x
                    decx
                    sta       bufdis,x
                    lda       #PUNFLE
                    add       contfle
                    tax
                    lda       #' '
                    sta       bufdis,x            ; borra la flecha
                    lda       sava                ; direccion de la opcion que ha encontrado igual
                    sta       contfle             ; pone la flecha en la opcion igual
                    jsr       trans               ; la va a transmitir para que se oiga
                    bclr      flags
                    bclr      1,flags
                    bclr      2,flags
                    bclr      3,flags
                    jmp       main

fin1                lda       #PUNAMO
                    add       contbuf
                    sta       savex               ; direc. de la opcion de amplif.(unidades)
                    lda       #PUNPAO
                    add       contbuf
                    sta       savex1              ; direc. de la opcion de pantall.(unidades)

acep4               ldx       #PUNAME             ; se¤ala el punto de las unidades
                    lda       bufdis,x
                    ldx       savex
                    sta       bufdis,x
                    ldx       #PUNAME
                    decx                          ; va para las decenas
                    lda       bufdis,x
                    ldx       savex
                    decx
                    sta       bufdis,x
          ;-------------------------------------- ; va a hacer lo mismo con las pantallas
                    ldx       #PUNPAE             ; se¤ala el punto de las unidades
                    lda       bufdis,x
                    ldx       savex1
                    sta       bufdis,x
                    ldx       #PUNPAE
                    decx                          ; va para las unidades
                    lda       bufdis,x
                    ldx       savex1
                    decx
                    sta       bufdis,x

ace00               lda       #'-'
                    ldx       #PUNAME
                    sta       bufdis,x            ; pone los '-' en los puntos de entrada
                    decx
                    sta       bufdis,x
                    ldx       #PUNPAE
                    sta       bufdis,x
                    decx
                    sta       bufdis,x
                    jsr       refdis
                    lda       contop
                    beq       ace03
                    cmpa      contfle
                    bne       ace04
ace03               jsr       trans               ; solo transmite si es la primera opcion
ace04               lda       contbuf
                    cmpa      #!15
                    beq       ace05
                    add       #03                 ; lo prepara para que apunte a la sig. opcion
                    sta       contbuf
                    bra       ace06

ace05               clr       contbuf
ace06               lda       contop              ; contador de opciones entradas
                    cmpa      #!15
                    beq       ace01               ; cuando llegue a 18 ya no se resetea,poque
                    add       #3                  ; siempre habra opciones para elegir
                    sta       contop
ace01               bclr      2,flags
                    bclr      flags
                    bclr      1,flags
                    jmp       main

acep2               lda       #'0'
                    ldx       #PUNAME
                    brclr     flags,acee          ; estoy en entrada de amp. o pant.
                    ldx       #PUNPAE
acee                decx
                    jmp       desp0

acep3
; hay ampl. entrado y pondr‚ las misma pantall. que estan sonando

                    lda       contop
                    beq       ma
                    lda       #PUNPAO
                    add       contfle             ; le suma dode esta la flecha
                    tax
                    stx       savex
                    lda       bufdis,x
                    ldx       #PUNPAE
                    sta       bufdis,x            ; le pone las mismas pantall. que hay
                    ldx       savex
                    decx
                    lda       bufdis,x
                    ldx       #PUNPAE
                    decx
                    jmp       desp0
ma                  jmp       main

; como no hay nada entrado pondr‚ el mismo ampli. que suena

acep5               lda       contop
                    beq       ma
                    lda       #PUNAMO
                    add       contfle             ; le suma dode esta la flecha
                    tax
                    stx       savex
                    lda       bufdis,x
                    ldx       #PUNAME
                    sta       bufdis,x            ; le pone el mismo ampli. que esta sonando
                    ldx       savex
                    decx
                    lda       bufdis,x
                    ldx       #PUNAME
                    decx
                    jmp       desp0

;*******************************************************************************
; va tirar un espacio atras y borra lo que hay

BORRA               proc
                    brset     2,flags,_1@@        ; la edicion esta completa
                    brset     1,flags,_2@@        ; solo hay una cifra
                    brset     flags,_3@@          ; hay ampif. entrado
                    bra       ma                  ; si no hay nada no puede borrar
          ;--------------------------------------
_1@@                ldx       #PUNPAE
                    decx
                    lda       bufdis,x            ; coje la de las decenas
                    incx
                    sta       bufdis,x            ; la pune como unidades
                    lda       #'-'
                    decx
                    sta       bufdis,x            ; en decenas pone el guion
                    jsr       refdis
                    bset      flags               ; sigue habiendo ampli. entrado
                    bset      1,flags             ; falta por entrar una cifra
                    bclr      2,flags             ; la edicion ya no esta completa
                    jmp       main
          ;--------------------------------------
_2@@                ldx       #PUNAME
                    brclr     flags,br2           ; mira si esta en amp. o en pant.
                    ldx       #PUNPAE
br2                 lda       #'-'
                    sta       bufdis,x
                    jsr       refdis
                    bclr      1,flags
                    bclr      2,flags
                    jmp       main
          ;-------------------------------------- ; hay ampli. entrado
_3@@                ldx       #PUNAME
                    decx
                    lda       bufdis,x            ; coje la de las decenas
                    incx
                    sta       bufdis,x            ; la pune como unidades
                    lda       #'-'
                    decx
                    sta       bufdis,x            ; en decenas pone el guion
                    jsr       refdis
                    bset      1,flags             ; falta por entrar una cifra
                    bclr      flags               ; la entrada de ampli. no estara completa
                    jmp       main

;*******************************************************************************

dret                proc
                    lda       contop              ; contador de opciones entradas
                    beq       Done@@
                    cmpa      #!15
                    beq       _1@@
                    sub       #3
_1@@                cmpa      contfle
                    beq       Done@@              ; ya esta al tope derecho de las opciones
                    lda       #PUNFLE
                    add       contfle
                    tax
                    lda       #' '
                    sta       bufdis,x
                    lda       contfle
                    add       #3
                    sta       contfle
                    jsr       trans
Done@@              jmp       main

;*******************************************************************************

esq                 proc
                    lda       contfle
                    beq       Done@@
                    lda       #PUNFLE
                    add       contfle
                    tax
                    lda       #' '
                    sta       bufdis,x
                    lda       contfle
                    sub       #03
                    sta       contfle
                    jsr       trans
Done@@              jmp       main

;*******************************************************************************
; Mira si hay pulsacion de teclado,y si la hay vuelve
; sin carry y con el dato en 'A' y si no la hay vuelve
; con carry

tecla               proc
                    stx       savex               ; guarda 'x'
                    mov       #$f0,PCA

                    bclr      1,PORTC             ; activa el buffer `74LS244'
                    mov       #$f0,PORTA

                    lda       PORTA
                    and       #$0f
                    beq       notec               ; No hay ninguna pulsacion
                    bsr       Delay               ; quiere eliminar rebotes

                    lda       PORTA
                    and       #$0f
                    beq       notec               ; No hay ninguna pulsacion
                    bsr       Delay               ; quiere eliminar rebotes

                    lda       PORTA
                    and       #$0f
                    beq       notec               ; No hay ninguna pulsacion
                    cmpa      savex1
                    beq       notec1
                    sta       savex1              ; lo guarda para no cojer la misma otra vez

                    ldx       #4
                    lda       #$10
          ;-------------------------------------- ; va a buscar que tecla ha sido
busc                sta       sava
                    sta       PORTA
                    lda       PORTA
                    and       #$0f
                    bne       busc1
                    lda       sava
                    lsla
                    dbnzx     busc

notec               clr       savex1
notec1              bset      1,PORTC             ; desactiva el `74LS244'
                    sec
                    ldx       savex
                    rts

busc1               lda       PORTA
                    clrx
busc2               cmpa      tabla6,x            ; va a buscar el caracter que corresponde
                    beq       sitec
                    incx:2
                    cpx       #$20
                    bne       busc2
                    bra       notec

sitec               incx
                    lda       tabla6,x
                    bset      1,PORTC             ; desactiva el `74LS244'
                    clc
                    ldx       savex
                    rts

;*******************************************************************************

Delay               proc
                    clra
                    bsr       Delay@@
Delay@@             dbnza     *
                    rts

;*******************************************************************************
; esta rutina convierte el contenido de 'A' en ASCII
; a a hexadecimal, dejando el reltado en los 4 bits bajos de 'A'
; y llamando a continuacion a 'pack' con otro carcter ASCII en A
; vuelve con los dos en binario en 'a' el prim. el MSD

hex                 proc
                    sta       sava
                    clr       savex
pack
                    cmpa      #$30                ; es menor que 30 ?
                    bcs       surt                ; si lo es sale con carry
                    cmpa      #$47                ; es mayor que 47 ?
                    bcc       surt                ; si lo es sale con carry
                    cmpa      #$3a                ; es menor que #10
                    bcs       pak1
                    cmpa      #$40                ; es mayor que #10 ?
                    bcs       surt
                    adc       #8                  ; suma 9 si el carry esta puesto
pak1
                    rola:4
                    stx       savex1              ; guarda 'x' para no machacar
                    ldx       #4
pak2
                    rola                          ; pasa 'a' a savex
                    rol       savex               ; introduce el carry
                    decx
                    bne       pak2
                    ldx       savex1              ; restaura 'x'lda savex
                    lda       savex
                    clc
                    rts

surt                sec
                    rts

;*******************************************************************************

trans               proc
                    lda       #PUNFLE
                    add       contfle
                    tax
                    lda       #!169
                    sta       bufdis,x            ; pone la flecha en la nueva posicion
                    bsr       refdis

                    lda       #PUNAMO
                    add       contfle
                    tax
                    decx
                    lda       bufdis,x            ; decenas de amplificadores
                    bsr       hex                 ; convierte a binario
                    bcs       nova
                    incx                          ; va para las unidades
                    lda       bufdis,x
                    bsr       pack
                    bcs       nova
                    sta       buftran+2

                    lda       #PUNPAO
                    add       contfle
                    tax
                    decx
                    lda       bufdis,x            ; decenas de pantallas
                    bsr       hex                 ; convierte a binario
                    bcs       nova
                    incx                          ; va para las unidades
                    lda       bufdis,x
                    bsr       pack
                    bcs       nova
                    sta       buftran+3
                    clrx
tra
                    lda       buftran,x
                    jsr       SciOut
                    incx
                    cpx       #04
                    bne       tra
nova                rts

;*******************************************************************************
; esta rutina sirve para refrescar el display

refdis              proc
                    jsr       home
                    clrx
Loop@@              lda       bufdis,x
                    bsr       print               ; saca texto preparado en RAM
                    incx
                    cpx       #84
                    bne       Loop@@
                    rts

;*******************************************************************************

pretim              proc
                    lda       TSR
                    lda       $19
                    lda       $18
                    rts

;*******************************************************************************
; subrutinas gestion display
; Envia un caracter al display
; El display esta dividido en 8 páginas

; -------------------------------------
; | página 0        |    página 4       |
; | página 1        |    página 5       |
; | página 2        |    página 6       |
; | página 3        |    página 7       |
; -------------------------------------

; Las paginas 0 y 4 componen la linea 1 del display, las 1 y 5 la linea 2 y asi sucesivamente.
; En cada pagina caben 64 bytes, por lo que cuando se acaba de llenar una pagina, tenemos que
; conmutar a la siguiente pagina para poder completar la lina, es decir, si acabamos de llenar
; la pagina 0, tendremos que continuar por la 4, para ello nos ayudamos de la tabla <paginas>,
; donde nos indica la pagina donde tenemos que continuar y el ancho en bytes de la pagina.
; En las paginas 4,5,6 y 7, escribiremos 62 bytes, porque como los caracteres son de 6 bytes,
; nos quedarian partidos por la mitad al cambiar de linea.

print               proc
                    stx       svx
                    cmpa      #!166
                    bls       n1
nn1                 jmp       tbl5

n1                  cmpa      #!132
                    bls       n2
nn2                 jmp       tbl4

n2                  cmpa      #!98
                    bls       n3
nn3                 bra       tbl3

n3                  cmpa      #!64
                    bls       n4
nn4                 bra       tbl2

n4                  cmpa      #!31
                    bhs       tbl
                    jmp       fprint

tbl                 sub       #$20
                    ldx       #6
                    mul
                    tax
                    clr       tmp2
                    jsr       GetWidth            ; Miramos el ancho de la pagina
bc
                    lda       tabla,x
                    jsr       DispChar
                    bcc       sgbc
                    jmp       fprint

sgbc                incx
                    inc       tmp2
                    inc       ncars
                    lda       ncars
                    cmpa      PgWidth             ; Cada 64 bytes es una pagina
                    bne       BcCnt
                    inc       pag
                    jsr       StPage
BcCnt               lda       tmp2

                    cmpa      #$6
                    bne       bc
                    jmp       fprint


tbl2
                    sub       #$41
                    ldx       #6
                    mul
                    tax
                    clr       tmp2
                    jsr       GetWidth            ; Miramos el ancho de la pagina
bc2
                    lda       tabla2,x
                    jsr       DispChar
                    bcc       no1
                    jmp       fprint

no1                 incx
                    inc       tmp2
                    inc       ncars
                    lda       ncars
                    cmpa      PgWidth             ; Cada 64 bytes es una pagina
                    bne       BcCnt2
                    inc       pag
                    jsr       StPage
BcCnt2              lda       tmp2
                    cmpa      #$6
                    bne       bc2
                    jmp       fprint


tbl3
                    sub       #'c'
                    ldx       #6
                    mul
                    tax
                    clr       tmp2
                    jsr       GetWidth            ; Miramos el ancho de la pagina
bc3
                    lda       tabla3,x
                    jsr       DispChar
                    bcc       no2
                    jmp       fprint

no2                 incx
                    inc       tmp2
                    inc       ncars
                    lda       ncars
                    cmpa      PgWidth             ; Cada 64 bytes es una pagina
                    bne       BcCnt3
                    inc       pag
                    bsr       StPage
BcCnt3              lda       tmp2
                    cmpa      #$6
                    bne       bc3
                    jmp       fprint


tbl4
                    sub       #!133
                    ldx       #6
                    mul
                    tax
                    clr       tmp2
                    bsr       GetWidth            ; Miramos el ancho de la pagina
bc4
                    lda       tabla4,x
                    jsr       DispChar
                    bcc       no3
                    bra       fprint

no3                 incx
                    inc       tmp2
                    inc       ncars
                    lda       ncars
                    cmpa      PgWidth             ; Cada 64 bytes es una pagina
                    bne       BcCnt4
                    inc       pag
                    bsr       StPage
BcCnt4              lda       tmp2
                    cmpa      #$6
                    bne       bc4
                    bra       fprint

tbl5
                    sub       #!167
                    ldx       #6
                    mul
                    tax
                    clr       tmp2
                    bsr       GetWidth            ; Miramos el ancho de la pagina
bc5
                    lda       tabla5,x
                    jsr       DispChar
                    bcc       no4
                    bra       fprint

no4                 incx
                    inc       tmp2
                    inc       ncars
                    lda       ncars
                    cmpa      PgWidth             ; Cada 64 bytes es una pagina
                    bne       BcCnt5
                    inc       pag
                    bsr       StPage
BcCnt5              lda       tmp2
                    cmpa      #$6
                    bne       bc5
                    bra       fprint


; ANTES DE CAMBIAR DE PÁGINA, MIRAMOS SI LA HEMOS LLENADO TODA, EN EL CASO DE NO HABERLO HECHO
; ENVIAMOS CEROS HASTA EL FINAL DE LA PÁGINA.

StPage
                    lda       ncars
                    cmpa      #!64
                    beq       sgPage
                    clra
                    bsr       DispChar
                    inc       ncars
                    bra       StPage

sgPage              pshx
                    ldx       pag
                    lda       #2
                    mul
                    tax
                    lda       paginas,x
                    jsr       SetPage
                    clr       ncars
                    clra
                    jsr       SetYPage            ; Ponemos la dirección Y a 0
                    pulx
                    bsr       GetWidth
                    rts

;*******************************************************************************

GetWidth            proc
                    pshx
                    ldx       pag
                    lda       #$2
                    mul
                    inca
                    tax
                    lda       paginas,x
                    sta       PgWidth
                    pulx
                    rts

fprint              proc
                    ldx       svx
                    rts

;*******************************************************************************
; GENERA UN BACK SPACE

BackSp              proc
                    lda       ncars
                    bne       NoCars

                    lda       pag
                    beq       FinBack

                    dec       pag
                    ldx       pag
                    lda       #2
                    mul
                    tax
                    lda       paginas,x
                    jsr       SetPage
                    incx
                    lda       paginas,x
                    deca
                    sub       #6
                    sta       ncars
                    jsr       SetYPage
                    bra       ncar

NoCars
                    lda       ncars
                    sub       #6
                    cmpa      #6
                    bpl       mmm
                    clra
mmm                 sta       ncars
                    jsr       SetYPage            ; Ponemos la direccion Y

ncar                lda       #' '
                    jsr       print
                    lda       ncars
                    sub       #6
                    sta       ncars
                    jsr       SetYPage            ; Ponemos la direccion Y
FinBack             rts

;*******************************************************************************
; Escribe un byte en el display
; El byte ha de venir en el acumulador

DispChar            proc
                    sta       sava
                    jsr       GetStatus           ; Obtenemos el estado del display.
                    brset     ST_BUSY,DispChar    ; Si está ocupado esperamos a que se libere.
                    brset     ST_RESET,Done@@     ; Si está en RESET, salimos con error.
                    brset     ST_ONOFF,Done@@     ; Si está desactivado sale con un error.

                    lda       #$ff
                    sta       DAT_DISP+4
                    lda       sava
                    jsr       Invierte
                    sta       DAT_DISP

                    bclr      CS,CTRL_DISP
                    nop:5
                    bset      E,CTRL_DISP
                    nop:5
                    bclr      WR,CTRL_DISP
                    nop:5
                    bset      DI,CTRL_DISP
                    nop:5

                    bclr      E,CTRL_DISP
                    nop:5
                    bset      CS,CTRL_DISP
                    nop:5

                    clc
Done@@              rts

;*******************************************************************************
; BORRA LA PANTALLA

Cls                 proc
                    clra
                    bsr       SetPage
                    clra
                    bsr       SetYPage

                    clr       ncars
                    clr       pag

Loop@@              lda       #' '
                    jsr       print
                    lda       pag
                    cmpa      #8
                    bne       Loop@@

                    clra
                    bsr       SetPage
                    clra
                    bsr       SetYPage
                    clr       ncars
                    clr       pag
                    rts

;*******************************************************************************

home                proc
                    clra
                    bsr       SetPage             ; Ponemos la pagina 0 de RAM
                    clra
                    bsr       SetYPage            ; Ponemos la dirección Y a 0
;                   clra
;                   jsr       SetLine             ; Indicamos que la primerla lína a visualizar sera la 0
                    clr       pag
                    clr       ncars
                    rts

;*******************************************************************************
; Initialize the display

InitDisp            proc
                    bset      WR,CTRL_DISP+4
                    nop:5
                    bset      DI,CTRL_DISP+4
                    nop:5
                    bset      RST,CTRL_DISP+4
                    nop:5
                    bset      CS,CTRL_DISP+4
                    nop:5
                    bset      E,CTRL_DISP+4
                    nop:5
                    bset      RST,CTRL_DISP       ; Activamos el RESET
                    nop:5
                    bsr       DispOn              ; Ponemos el display en marcha
                    bsr       home
                    rts

;*******************************************************************************
; Rutina para activar el display

DispOn              proc
                    lda       #$3f                ; Comando para activar el display
                    jsr       Invierte
                    bra       WrtCtrl

;*******************************************************************************

DispOff             proc
                    lda       #$3e                ; Comando para desactivar el display
                    jsr       Invierte
                    bra       WrtCtrl

;*******************************************************************************
; Pone la dirección x de la pagina RAM.
; El número de pagina ha de venir en el acumulador el rango es de 0-7

SetPage             proc
                    cmpa      #7
                    bhi       Done@@
                    ora       #$b8
                    jsr       Invierte
                    bra       WrtCtrl
Done@@              equ       :AnRTS

;*******************************************************************************
; Pone la dirección y de la RAM
; El número de pagina ha de venir en el acumulador el rango es de 0-3f

SetYPage            proc
                    ora       #$40
                    jsr       Invierte
                    bra       WrtCtrl

;*******************************************************************************
; Pone la primera línea a mostrar en el display.
; El número de línea ha de estar en el acumulador, el rango es de 0-3f

SetLine             proc
                    ora       #$c0
                    jsr       Invierte
;                   bra       WrtCtrl

;*******************************************************************************
; Rutina para enviar un comando de control al display
; El comando ha de estar en el acumulador

WrtCtrl             proc
                    psha

Loop@@              bsr       GetStatus           ; Obtenemos el estado del display
                    brset     ST_BUSY,Loop@@      ; Si está ocupado esperamos a que se libere
                    brset     ST_RESET,Fail@@     ; Si está en RESET, salimos con error

                    mov       #$ff,DAT_DISP+4     ; Pone el puerto de datos como salidas
                    nop:16

                    bclr      WR,CTRL_DISP
                    nop:5
                    bclr      DI,CTRL_DISP
                    nop:5
                    bclr      CS,CTRL_DISP        ; Seleccionamos el DISPLAY
                    nop:5
                    bclr      E,CTRL_DISP         ; Lo habilitamos
                    nop:5

                    pula
                    sta       DAT_DISP

                    bset      E,CTRL_DISP
                    nop:5
                    bset      CS,CTRL_DISP
                    nop:5

                    clc
                    rts

Fail@@              pula
                    bset      E,CTRL_DISP         ; Lo habilitamos
                    nop:5
                    bset      CS,CTRL_DISP
                    nop:5
                    sec
                    rts

;*******************************************************************************
; Miramos el estado del display
; En el flags está el estado del display

GetStatus           proc
                    clr       DAT_DISP+4          ; Ponemos el puerto de datos como entradas
                    bclr      DI,CTRL_DISP
                    nop:5
                    bset      WR,CTRL_DISP
                    nop:5
                    bclr      CS,CTRL_DISP
                    nop:5
                    bclr      E,CTRL_DISP
                    nop:5
                    bset      E,CTRL_DISP
                    nop:5

                    lda       DAT_DISP
                    and       #$e0
                    ora       flags
                    sta       flags
                    bset      CS,CTRL_DISP
                    rts

;*******************************************************************************

Invierte            proc
                    pshx
                    lsla
                    rorx
                    lsla
                    rorx
                    lsla
                    rorx
                    lsla
                    rorx
                    lsla
                    rorx
                    lsla
                    rorx
                    lsla
                    rorx
                    lsla
                    rorx
                    txa
                    pulx
                    rts

;*******************************************************************************
; Configura el puerto SCI
; En A ha de estar el valor del registro SCCR1.
; En X ha de estar el valor del registro BAUD. (velocidad)

SciInit             proc
                    sta       SCCR1
                    lda       #$0C                ; - TDRE interrupt disabled
                                                  ; - TC interrupt disabled
                                                  ; - SCI interrupt disabled
                                                  ; - IDLE interrupt disbaled
                                                  ; - After the last byte is transmitted,the TDO line
                                                  ; becomes a hight-impedance line.
                                                  ; - Receiver disabled and RDRF,IDLE,OR,NF and FE
                                                  ; status bits are inhibited.
                    sta       SCCR2
                    stx       BAUD
                    rts

;*******************************************************************************
; DESACTIVA EL MODO SCI

SciOff              proc
                    lda       #$00
                    sta       SCCR1               ; one start bit,eight data bits, one stop bit

                    lda       #$00                ; - TDRE interrupt disabled
                                                  ; - TC interrupt disabled
                                                  ; - SCI interrupt enabled
                                                  ; - IDLE interrupt disbaled
                                                  ; - Transmiter enabled
                                                  ; - Receiver enabled
                                                  ; status bits are inhibited.
                    sta       SCCR2
                    rts

;*******************************************************************************
; TRANSMITE AL PC EL CONTENIDO DEL REGISTRO A

SciOut              proc
                    brclr     7,SCSR,*            ; MIRA SI ESTA OCUPADO
                    sta       SCDAT
                    brclr     6,SCSR,*            ; ESPERA PARA ACABAR DE TRANSMITIR
                    rts

;*******************************************************************************

SciIn               proc
                    brclr     5,SCSR,*            ; ESPERA QUE ENTREN UN CARACTER
                    lda       SCDAT               ; EN A ESTA EL CARACTER ENTRADO
                    rts

;*******************************************************************************
; Characters table
;*******************************************************************************

tabla               fcb       $00,$00,$00,$00,$00,$00  ; SPACE
                    fcb       $00,$06,$5f,$06,$00,$00  ; !
                    fcb       $07,$03,$00,$07,$03,$00  ; "
                    fcb       $24,$7e,$24,$7e,$24,$00  ; #
                    fcb       $00,$24,$2b,$6a,$12,$00  ; $
                    fcb       $63,$13,$08,$64,$63,$00  ; %
                    fcb       $00,$00,$00,$00,$00,$00  ; &
                    fcb       $00,$00,$07,$03,$00,$00  ; '
                    fcb       $00,$00,$3e,$41,$00,$00  ; (
                    fcb       $00,$00,$41,$3e,$00,$00  ; )
                    fcb       $04,$1f,$0e,$1f,$04,$00  ; ;
                    fcb       $08,$08,$3e,$08,$08,$00  ; +
                    fcb       $00,$00,$00,$00,$00,$00  ; ´
                    fcb       $08,$08,$08,$08,$08,$00  ; -
                    fcb       $00,$00,$60,$60,$00,$00  ; .
                    fcb       $20,$10,$08,$04,$02,$00  ; /
                    fcb       $3e,$51,$49,$45,$3e,$00  ; 0
                    fcb       $00,$42,$7f,$40,$00,$00  ; 1
                    fcb       $62,$51,$49,$49,$46,$00  ; 2
                    fcb       $22,$49,$49,$49,$36,$00  ; 3
                    fcb       $18,$14,$12,$7f,$10,$00  ; 4
                    fcb       $2f,$49,$49,$49,$31,$00  ; 5
                    fcb       $3c,$4a,$49,$49,$30,$00  ; 6
                    fcb       $01,$71,$09,$05,$03,$00  ; 7
                    fcb       $36,$49,$49,$49,$36,$00  ; 8
                    fcb       $06,$49,$49,$29,$1e,$00  ; 9
                    fcb       $00,$00,$6c,$6c,$00,$00  ; :
                    fcb       $00,$00,$76,$36,$00,$00  ; ;
                    fcb       $00,$08,$14,$22,$41,$00  ; <
                    fcb       $24,$24,$24,$24,$24,$00  ; =
                    fcb       $00,$41,$22,$14,$08,$00  ; >
                    fcb       $02,$01,$59,$09,$06,$00  ; ?
                    fcb       $3e,$41,$5d,$55,$1e,$00  ; @
tabla2              fcb       $7e,$11,$11,$11,$7e,$00  ; A
                    fcb       $7f,$49,$49,$49,$36,$00  ; B
                    fcb       $3E,$41,$41,$41,$22,$00  ; C
                    fcb       $7f,$41,$41,$41,$3e,$00  ; D
                    fcb       $7f,$49,$49,$49,$41,$00  ; E
                    fcb       $7f,$09,$09,$09,$01,$00  ; F
                    fcb       $3e,$41,$49,$49,$7a,$00  ; G
                    fcb       $7f,$08,$08,$08,$7f,$00  ; H
                    fcb       $41,$7f,$41,$00,$00,$00  ; I
                    fcb       $30,$40,$40,$40,$3f,$00  ; J
                    fcb       $7f,$08,$14,$22,$41,$00  ; K
                    fcb       $7f,$40,$40,$40,$40,$00  ; L
                    fcb       $7f,$02,$04,$02,$7f,$00  ; M
                    fcb       $7f,$02,$04,$08,$7f,$00  ; N
                    fcb       $3e,$41,$41,$41,$3e,$00  ; O
                    fcb       $7f,$09,$09,$09,$06,$00  ; P
                    fcb       $3e,$41,$61,$21,$5e,$00  ; Q
                    fcb       $7f,$09,$09,$19,$66,$00  ; R
                    fcb       $26,$49,$49,$49,$32,$00  ; S
                    fcb       $01,$01,$7f,$01,$01,$00  ; T
                    fcb       $7f,$40,$40,$40,$7f,$00  ; U
                    fcb       $1f,$20,$40,$20,$1f,$00  ; V
                    fcb       $7f,$40,$3c,$40,$7f,$00  ; W
                    fcb       $63,$14,$08,$14,$63,$00  ; X
                    fcb       $07,$08,$70,$08,$07,$00  ; Y
                    fcb       $71,$49,$45,$43,$00,$00  ; Z
                    fcb       $7f,$41,$41,$00,$00,$00  ; [
                    fcb       $01,$02,$04,$08,$10,$00  ; \
                    fcb       $41,$41,$7f,$00,$00,$00  ; ]
                    fcb       $04,$02,$01,$02,$04,$00  ; ^
                    fcb       $80,$80,$80,$80,$80,$80  ; _
                    fcb       $03,$07,$00,$00,$00,$00  ; `
                    fcb       $20,$54,$54,$54,$78,$00  ; a
                    fcb       $7f,$44,$44,$44,$38,$00  ; b
tabla3              fcb       $38,$44,$44,$44,$28,$00  ; c
                    fcb       $38,$44,$44,$44,$7f,$00  ; d
                    fcb       $38,$54,$54,$54,$08,$00  ; e
                    fcb       $08,$fe,$09,$09,$00,$00  ; f
                    fcb       $18,$a4,$a4,$a4,$7c,$00  ; g
                    fcb       $7f,$04,$04,$78,$00,$00  ; h
                    fcb       $00,$00,$7d,$40,$00,$00  ; i *
                    fcb       $04,$08,$84,$7d,$00,$00  ; j
                    fcb       $7f,$10,$28,$44,$00,$00  ; k
                    fcb       $00,$00,$7F,$40,$00,$00  ; l *
                    fcb       $7c,$04,$18,$04,$78,$00  ; m *
                    fcb       $00,$7c,$04,$04,$78,$00  ; n
                    fcb       $38,$44,$44,$44,$38,$00  ; o
                    fcb       $fc,$44,$44,$44,$38,$00  ; p
                    fcb       $38,$44,$44,$44,$fc,$00  ; q
                    fcb       $44,$78,$44,$04,$08,$00  ; r *
                    fcb       $08,$54,$54,$54,$20,$00  ; s
                    fcb       $04,$3e,$44,$24,$00,$00  ; t *
                    fcb       $3c,$40,$20,$7c,$00,$00  ; u
                    fcb       $1c,$20,$40,$20,$1c,$00  ; v
                    fcb       $3c,$60,$30,$60,$3c,$00  ; w
                    fcb       $6c,$10,$10,$6c,$00,$00  ; x
                    fcb       $9c,$a0,$60,$9c,$00,$00  ; y
                    fcb       $64,$58,$58,$4c,$00,$00  ; z
                    fcb       $08,$3e,$41,$41,$00,$00  ; { ALT- 0123
                    fcb       $00,$00,$77,$00,$00,$00  ; | ALT- 0124
                    fcb       $41,$41,$3e,$08,$00,$00  ; } ALT- 0125
                    fcb       $02,$01,$02,$01,$00,$00  ; ~ ALT- 0126
                    fcb       $3c,$26,$23,$26,$3c,$00  ;   ALT- 0127
                    fcb       $17,$a1,$e1,$21,$12,$00  ;   ALT- 0128
                    fcb       $3d,$40,$20,$3d,$00,$00  ;   ALT- 0129
                    fcb       $38,$54,$54,$55,$09,$00  ;   ALT- 0130
                    fcb       $20,$55,$55,$55,$78,$00  ;   ALT- 0131
                    fcb       $20,$55,$54,$55,$78,$00  ;   ALT- 0132
tabla4              fcb       $20,$55,$55,$54,$78,$00  ;   ALT- 0133
                    fcb       $20,$57,$55,$57,$78,$00  ;   ALT- 0134
                    fcb       $1c,$a2,$e2,$a2,$14,$00  ;   ALT- 0135
                    fcb       $38,$55,$55,$55,$08,$00  ;   ALT- 0136
                    fcb       $38,$55,$54,$55,$97,$00  ;   ALT- 0137
                    fcb       $38,$55,$55,$54,$97,$00  ;   ALT- 0138
                    fcb       $00,$01,$7c,$41,$00,$00  ;   ALT- 0139
                    fcb       $00,$02,$79,$42,$00,$00  ;   ALT- 0140
                    fcb       $00,$01,$7c,$20,$00,$00  ;   ALT- 0141
                    fcb       $70,$29,$24,$29,$70,$00  ;   ALT- 0142
                    fcb       $78,$2f,$25,$2f,$78,$00  ;   ALT- 0143
                    fcb       $7c,$54,$54,$55,$45,$00  ;   ALT- 0144
                    fcb       $34,$54,$7c,$54,$58,$00  ;   ALT- 0145
                    fcb       $7e,$09,$7f,$49,$49,$00  ;   ALT- 0146
                    fcb       $38,$45,$45,$39,$00,$00  ;   ALT- 0147
                    fcb       $38,$45,$44,$39,$00,$00  ;   ALT- 0148
                    fcb       $39,$45,$44,$39,$00,$00  ;   ALT- 0149
                    fcb       $3c,$41,$21,$7e,$00,$00  ;   ALT- 0150
                    fcb       $3d,$41,$20,$7c,$00,$00  ;   ALT- 0151
                    fcb       $9c,$a1,$60,$3d,$00,$00  ;   ALT- 0152
                    fcb       $3d,$42,$42,$3D,$00,$00  ;   ALT- 0153
                    fcb       $3c,$41,$40,$3d,$00,$00  ;   ALT- 0154
                    fcb       $18,$24,$66,$24,$00,$00  ;   ALT- 0155
                    fcb       $48,$3e,$49,$49,$62,$00  ;   ALT- 0156
                    fcb       $29,$2a,$7c,$2a,$29,$00  ;   ALT- 0157
                    fcb       $7f,$09,$16,$78,$00,$00  ;   ALT- 0158
                    fcb       $40,$88,$7e,$09,$02,$00  ;   ALT- 0159
                    fcb       $20,$54,$55,$55,$78,$00  ;   ALT- 0160
                    fcb       $00,$00,$7d,$41,$00,$00  ;   ALT- 0161
                    fcb       $00,$38,$44,$46,$39,$00  ;   ALT- 0162
                    fcb       $3c,$40,$21,$7d,$00,$00  ;   ALT- 0163
                    fcb       $7a,$09,$0a,$71,$00,$00  ;   ALT- 0164
                    fcb       $7a,$11,$22,$79,$00,$00  ;   ALT- 0165
                    fcb       $08,$55,$55,$55,$5e,$00  ;   ALT- 0166

tabla5              fcb       $4e,$51,$51,$4e,$00,$00  ;   ALT - 167
                    fcb       $30,$48,$4d,$40,$20,$00  ;   ALT - 168
                    fcb       $04,$06,$7f,$06,$04,$00  ; FLECHA ARRIBA ALT - 169
                    fcb       $10,$30,$7f,$30,$10,$00  ; FLECHA ABAJO ALT - 170
                    fcb       $00,$20,$70,$a8,$20,$37  ; DIBUJO DE INTRO ALT - 171

paginas             fcb       0,64
                    fcb       4,62
                    fcb       1,64
                    fcb       5,62
                    fcb       2,64
                    fcb       6,62
                    fcb       3,64
                    fcb       7,62

;*******************************************************************************
; R O M   D A T A
;*******************************************************************************

texto               fcb       '   **** COMPAX ****  '
                    fcb       '                     '
                    fcb       'Electr'
                    fcb       !162                ; o
                    fcb       'nica Barcelona'
                    fcb       $00

texto1              fcb       'Amp>-                '
                    fcb       'Pan>-                '
                    fcb       'Fun. '
                    fcb       !169
                    fcb       '               '
                    fcb       'Amplif.--  Pantall.--'
                    fcb       $00
tabla6
                    fcb       $11,'0',$21,'C',$41,'E',$81,$0D,$12,'8',$22,'7',$42,'9'
                    fcb       $82,'D',$14,'5'
                    fcb       $24,'4',$44,'6',$84,'B',$18,'2',$28,'1',$48,'3',$88,'A'

;*******************************************************************************
; Interrupt handlers
;*******************************************************************************

timer
                    brclr     4,flags,saltim
                    dec       contim              ; contador para hacer bucles
                    bne       saltim
                    bclr      4,flags
saltim              jsr       pretim
AnRTI               rti

;*******************************************************************************

IRQ                 def       AnRTI
SPI                 def       AnRTI
SCI                 def       AnRTI
SWI                 def       AnRTI

;*******************************************************************************
                    #VECTORS  $1FF4
;*******************************************************************************

                    fdb       SPI
                    fdb       SCI
                    fdb       timer
                    fdb       IRQ
                    fdb       SWI
                    fdb       ENTRY

;*******************************************************************************
; Config Registers
;                   org       $1ff0
;                   fdb       0

;*******************************************************************************
                    end
