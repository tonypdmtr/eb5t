;
;
;                NOMBRE       : COMPAX.ASM
;                DESCRIPCION  : Comparardor de Amplificadores y Pantallas acust.
;                LENGUAJE     : Ensamblador para el motorola m68705C8
;                EDITOR       : EMETERIO
;                OBSERVACIONES:


**************************** definici¢n de flags ***************************

;flags      bit 0 ,si esta a '1' es que ya tiene n§ de aplif. entrado
;            "  1 ,se pone cuando han entrado la primera puls. de una opci¢n
;            "  2 ,se pone cuando se ha acabado de editar
;            "  3 ,se pone en comp. cuando va a comparar pantallas
;            "  4 ,lo quita en timer al llegar a cero contim
;            "  5 ,---,
;            "  6 ,---;---- se usan en rutinas del display (Oscar)
;            "  7 ,---;
;
;flag1     bit 0 ,lo pone cuando hay una puls. de teclado y lo resetea si no
************************ definici¢n del bufer de transmisi¢n*****************

;            [EC]-[A1]-[amplif]-[pantall]  4 bytes todo en binario
;
*****************************************************************************
;
; DEFINICION DE LOS PORTS
PORTA   equ 00h
PORTB   equ 01h
PORTC   equ 02h
PORTD   equ 03h
PCA     equ 04h
PCB     equ 05h
PCC     equ 06h
PCD     equ 07h

SCDAT   equ 11h       ; Serial Comunications Data Register
SCCR1   equ 0eh       ; Serial Comunication Register 1
SCCR2   equ 0fh       ; Serial Comunication Register 2
SCSR    equ 10h       ; Serial comunication Status Register
BAUD    equ 0000dh    ; Baud Rate Register
TCR      equ   12h    ; Timer Control Register
TSR      equ   13h    ; Timer Status Register
SPCR     equ   0ah    ; spcr (serial peripheral control register)
SPSR     equ   0bh    ; Serial Peripheral Status Register
SPDR     equ   0ch    ; Serial Peripheral Data I/O Register
TCRH     equ   001ah  ;timer count register (high)
TCRL     equ   001bh  ;timer count register (low)
ENTRY    equ   100h

E        equ 7          ;Bit del puerto donde está conectada la línea E
WR       equ 5          ;Bit del puerto donde está conectada la línea WR
DI       equ 4          ;Bit del puerto donde está conectada la línea RS
RST      equ 6
CS       equ 3

ST_RESET equ 7
ST_ONOFF equ 6
ST_BUSY  equ 5

CTRL_DISP equ 02     ;PUERTO DONDE ESTÁN CONECTADAS LAS LÍNEAS DE CONTROL
DAT_DISP equ 00      ;PUERTO DONDE ESTÁN CONECTADAS LAS LÍNEAS DE DATOS

************************************ CONSTANTES *****************************

PUNAMO  equ !05  ;puntero de princ. de mem. de amplif. (decenas)
PUNPAO  equ !26  ;   "    "    "    "   "   "  pantallas (decenas)
PUNFLE  equ !47  ;   "    "  la flecha de elementos en funcionamiento
PUNAME  equ !71  ;   "    "  donde pondra lo que entren de num. de amplif.
PUNPAE  equ !83  ;   "    "  donde pondra lo que entren de num. de pantallas

;*********************************** RAM ************************************

                        org 30h
                ;(cada 64 es una página)
PgWidth rmb 01
bufdis  rmb 84          ;bufer del display
contim  rmb 1           ;contador para tiempos del timer
savex   rmb 1           ;guarda 'x' en tecla
savex1  rmb 1           ;para operar punteros en "acep"
contbuf  rmb 1           ;contador de las opciones entradas
contfle  rmb 1           ;puntero de la flecha a sumar con PUNFLE
buftran rmb 4           ;buffer de transmision via RS-485
contop  rmb 1           ;contador de opciones entradas
flags   rmb 1           ;definido arriba
flag1   rmb 1

sava    rmb 01
savx    rmb 01
svx     rmb 01
savx2   rmb 01
tmp2    rmb 01

pag     rmb 01  ;Página actual.
ncars   rmb 01  ;Número de bytes escritos en la página actual
tmp3    rmb 01

;*********************************** ROM ************************************

                         org 100h



        sei
        rsp             ;reset stack pointer
        lda #$80
        sta $1fdf       ;carga el option register
        clra
        clrx
cero:
        sta 30h,x       ;memoria ram a cero
        incx
        cpx #$ff
        bne cero
***************************INICIALIZACION DE LOS PORTS*************************

        lda #$f0
        sta PCA         ;cuatro byts bajos ent. los altos entr. (teclado)
        lda #$ff
        sta PCC
        sta PCB
        clra
        sta PORTA
        sta PORTB
        sta PORTC
        sta PCD

        clra
        ldx #$33        ;velocidad de transmision 1200 Bds.
        jsr SciInit     ;inicializa el port serie
*******************************************************************************

        bset 4,PORTB    ;salida (RS-485) activada
        bset 1,PORTC ;74ls244 desactivado

        jsr inidis      ;Inicializa el display
        jsr DispOn      ;Activa el display
        jsr cls         ;Borra el display
        jsr pretim      ;prepera el timer
        bset 5,TCR

        cli

        clrx
carg:
        lda texto1,x
        cmp #00
        beq carg0
        sta bufdis,x    ;carga el texto en RAM para poder operar
        incx
        bra carg

carg0:
        lda #$EC
        sta buftran
        lda #$A1
        sta buftran+1   ;carga el frame en el buffer de transmision

        clrx
res2:
        lda texto,x
        cmp #00
        beq res4
        jsr print               ;saca pantalla de inicio
        incx
        bra res2
res4:
        lda #20
        sta contim
        bset 4,flags
res1:
        brset 4,flags,res1
        sei
        clrx

        jsr refdis              ;saca a display el formato de funcionamiento
        bra main

borr:
        jmp borra               ;tira un espacio atras y borra lo que hay

dreta:
        jmp dret
esquer:
        jmp esq

main1:
        bclr 0,flag1
main:

        jsr tecla
        bcs main1
        brset 0,flag1,main  ;si est  es que aun es la misma pulsaci¢n
        bset 0,flag1        ;hay una puls. y no coje ninguna m s si hay flag
        cmp #'A'
        beq dreta               ;quiere escojer una opcion de la derecha
        cmp #'B'
        beq esquer              ;  "       "     "    "    "  "  izquierda
        cmp #'C'
        beq borr
        cmp #$0d
        beq acepta              ;return
        cmp #$3a
        bpl main
        brset 2,flags,main      ;si est  el bufer lleno, no se puede ent. m s
        sta sava                ;guarda el num. entrado
        brset 0,flags,pant      ;si esta puesto, ya hay amplif. entrado
        ldx #PUNAME              ;punto de entrada de edicion del ampli.
        brset 1,flags,desp      ;esta es la segunda cifra de una opci¢n
        sta bufdis,x
main0:
        bset 1,flags            ;falta la segunda cifra
        jsr refdis              ;saca todo al display
        bra main

desp:
        lda bufdis,x            ;coje la primera cifra para correrla (decenas)
        decx
        sta bufdis,x
        incx
        lda sava                ;recupera el num. que han entrado
desp0:
        sta bufdis,x
        jsr refdis
        bclr 1,flags            ;la segunda pluls. ya ha llegado
        brclr 0,flags,desp1
        bset 2,flags            ;la edici¢n est  cmpleta, solo return o borr.
        bclr 0,flags
        bra main
desp1:
        bset 0,flags            ;ahora falta que entren pantallas
        bra main

pant:

        ldx #PUNPAE
        brset 1,flags,desp      ;esta es la segunda cifra de una opci¢n
        sta bufdis,x
        bra main0
acp2:
        jmp acep2
acp3:
        jmp acep3

acepta:
                ;han pulsado return, y lo que hay en la linea de edici¢n
                ;lo va a pasar a las lineas superores y ejecuar la opcion


        brset 2,flags,acep1    ;si la edici¢n est  completa, lo emitir 
        brset 1,flags,acp2     ;solo hay una cifra, pondr  cero en decenas
        brset 0,flags,acp3      ;hay ampif. entrado, pondr‚ las mismas pant.
        jmp acep5               ;no hay nada, pondra el amp. que esta sonando
        lda contbuf
        beq main

acep1:
        lda contop
        bne ace                 ;¨es la primera opci¢n?
        lda #PUNAMO
        sta savex
        lda #PUNPAO
        sta savex1
        jmp acep4

ace:
        lda #PUNAMO             ;va a comparar si ya existe esta opcion
        sta savex
        lda #PUNAME
        sta savex1
        bclr 3,flags            ;este se pondr  cuando ya haya visto los ampli.
        clr sava                ;contador de opciones comprobadas
comp:
        ldx savex
        lda bufdis,x            ;coje la primera opc. para comp.
        ldx savex1
        cmp bufdis,x            ;compara las unidades
        bne noig
        ldx savex
        decx
        lda bufdis,x            ;compara las decenas
        ldx savex1
        decx
        cmp bufdis,x
        bne noig
        bra siigu
noig:
        bclr 3,flags
        lda sava
        cmp contop              ;mira si ya ha llegado al final
        beq fin1
        add #03
        sta sava
        lda #PUNAMO
        add sava
        sta savex
        lda #PUNAME
        sta savex1
        bra comp

;        lda savex
;        add #03
;        sta savex
;        bra comp

siigu:
                        ;ha encntrado el amplif. igual, va a mirar los altav.

        brset 3,flags,fin      ;si es '1' es que hay una opcion igual
        lda sava
        bne siigu1
        lda #PUNPAO
siigu2:
        sta savex
        lda #PUNPAE     ;va a comp. las pantall. de la misma opcion
        sta savex1
        bset 3,flags
        bra comp
siigu1:
        lda #PUNPAO
        add sava
        bra siigu2

fin:
                        ;hay una opci¢n igual que la entrada £ltima
                        ;no la pongo en el buffer, la voy a ejecutar

        lda #'-'
        ldx #PUNAME
        sta bufdis,x            ;pone los '-' en los puntos de entrada
        decx
        sta bufdis,x
        ldx #PUNPAE
        sta bufdis,x
        decx
        sta bufdis,x
        lda #PUNFLE
        add contfle
        tax
        lda #' '
        sta bufdis,x    ;borra la flecha
        lda sava        ;direccion de la opcion que ha encontrado igual
        sta contfle     ;pone la flecha en la opcion igual
        jsr trans       ;la va a transmitir para que se oiga
        bclr 0,flags
        bclr 1,flags
        bclr 2,flags
        bclr 3,flags
        jmp main

fin1:

        lda #PUNAMO
        add contbuf
        sta savex               ;direc. de la opcion de amplif.(unidades)
        lda #PUNPAO
        add contbuf
        sta savex1              ;direc. de la opcion de pantall.(unidades)

acep4:
        ldx #PUNAME             ;se¤ala el punto de las unidades
        lda bufdis,x
        ldx savex
        sta bufdis,x
        ldx #PUNAME
        decx                     ;va para las decenas
        lda bufdis,x
        ldx savex
        decx
        sta bufdis,x
                                ;va a hacer lo mismo con las pantallas
        ldx #PUNPAE             ;se¤ala el punto de las unidades
        lda bufdis,x
        ldx savex1
        sta bufdis,x
        ldx #PUNPAE
        decx                    ;va para las unidades
        lda bufdis,x
        ldx savex1
        decx
        sta bufdis,x

ace00:
        lda #'-'
        ldx #PUNAME
        sta bufdis,x            ;pone los '-' en los puntos de entrada
        decx
        sta bufdis,x
        ldx #PUNPAE
        sta bufdis,x
        decx
        sta bufdis,x
        jsr refdis
        lda contop
        beq ace03
        cmp contfle
        bne ace04
ace03:
        jsr trans               ;solo transmite si es la primera opcion
ace04:
        lda contbuf
        cmp #!15
        beq ace05
        add #03                 ;lo prepara para que apunte a la sig. opcion
        sta contbuf
        bra ace06
ace05:
        clr contbuf
ace06:
        lda contop              ;contador de opciones entradas
        cmp #!15
        beq ace01               ;cuando llegue a 18 ya no se resetea,poque
        add #03                 ;siempre habr  opciones para elegir
        sta contop
ace01:
        bclr 2,flags
        bclr 0,flags
        bclr 1,flags
        jmp main

acep2:
        lda #'0'
        ldx #PUNAME
        brclr 0,flags,acee      ;estoy en entrada de amp. o pant.
        ldx #PUNPAE
acee:
        decx
        jmp desp0


acep3:
               ;hay ampl. entrado y pondr‚ las misma pantall. que estan sonando

        lda contop
        beq ma
        lda #PUNPAO
        add contfle             ;le suma dode est  la flecha
        tax
        stx savex
        lda bufdis,x
        ldx #PUNPAE
        sta bufdis,x            ;le pone las mismas pantall. que hay
        ldx savex
        decx
        lda bufdis,x
        ldx #PUNPAE
        decx
        jmp desp0
ma:
        jmp main

acep5:
                ;como no hay nada entrado pondr‚ el mismo ampli. que suena

        lda contop
        beq ma
        lda #PUNAMO
        add contfle             ;le suma dode est  la flecha
        tax
        stx savex
        lda bufdis,x
        ldx #PUNAME
        sta bufdis,x            ;le pone el mismo ampli. que esta sonando
        ldx savex
        decx
        lda bufdis,x
        ldx #PUNAME
        decx
        jmp desp0


BORRA:
                ;va tirar un espacio atras y borra lo que hay

        brset 2,flags,borr1     ; la edici¢n est  completa
        brset 1,flags,borr2     ;solo hay una cifra
        brset 0,flags,borr3     ;hay ampif. entrado
        bra ma                  ;si no hay nada no puede borrar

borr1:

        ldx #PUNPAE
        decx
        lda bufdis,x            ;coje la de las decenas
        incx
        sta bufdis,x            ;la pune como unidades
        lda #'-'
        decx
        sta bufdis,x            ;en decenas pone el guion
        jsr refdis
        bset 0,flags            ;sigue habiendo ampli. entrado
        bset 1,flags            ;falta por entrar una cifra
        bclr 2,flags            ;la edicion ya no esta completa
        jmp main

borr2:
        ldx #PUNAME
        brclr 0,flags,br2       ;mira si esta en amp. o en pant.
        ldx #PUNPAE
br2:
        lda #'-'
        sta bufdis,x
        jsr refdis
        bclr 1,flags
        bclr 2,flags
        jmp main

borr3:
                ;hay ampli. entrado

        ldx #PUNAME
        decx
        lda bufdis,x            ;coje la de las decenas
        incx
        sta bufdis,x            ;la pune como unidades
        lda #'-'
        decx
        sta bufdis,x            ;en decenas pone el guion
        jsr refdis
        bset 1,flags            ;falta por entrar una cifra
        bclr 0,flags            ;la entrada de ampli. no estar  completa
ma1:
        jmp main

dret:
        lda contop              ;contador de opciones entradas
        beq ma1
        cmp #!15
        beq dret1
        sub #03
dret1:
        cmp contfle
        beq ma1                 ;ya esta al tope derecho de las opciones
        lda #PUNFLE
        add contfle
        tax
        lda #' '
        sta bufdis,x
        lda contfle
        add #03
        sta contfle
        jsr trans
        jmp main

esq:
        lda contfle
        beq ma1
        lda #PUNFLE
        add contfle
        tax
        lda #' '
        sta bufdis,x
        lda contfle
        sub #03
        sta contfle
        jsr trans
        jmp main



;;;;;**************************************************************************

tecla:
                        ;Mira si hay pulsaci¢n de teclado,y si la hay vuelve
                        ;sin carry y con el dato en 'A' y si no la hay vuelve
                        ;con carry

        stx savex               ;guarda 'x'
        lda #$f0
        sta PCA

        bclr 1,PORTC            ;activa el buffer `74LS244'
        lda #$f0
        sta PORTA
        lda PORTA
        and #$0f
        beq notec               ;No hay ninguna pulsacion
        jsr bucle               ;quiere eliminar rebotes
        lda PORTA
        and #$0f
        beq notec               ;No hay ninguna pulsacion
        jsr bucle               ;quiere eliminar rebotes
        lda PORTA
        and #$0f
        beq notec               ;No hay ninguna pulsacion
        cmp savex1
        beq notec1
        sta savex1               ;lo guarda para no cojer la misma otra vez
        ldx #04
        lda #$10
busc:
                                ;va a buscar que tecla ha sido
        sta sava
        sta PORTA
        lda PORTA
        and #$0f
        bne busc1
        lda sava
        lsla
        decx
        bne busc
notec:
        clr savex1
notec1:
        bset 1,PORTC            ;desactiva el `74LS244'
        sec
        ldx savex
        rts

busc1:
        lda PORTA
        clrx
busc2:
        cmp tabla6,x             ;va a buscar el caracter que corresponde
        beq sitec
        incx
        incx
        cpx #$20
        bne busc2
        bra notec
sitec:
        incx
        lda tabla6,x
        bset 1,PORTC            ;desactiva el `74LS244'
        clc
        ldx savex
        rts

                        ;****************************

bucle:
        clra
        jsr buc
buc:
        deca
        bne buc
        rts

                        ;******************************

hex:
               ;esta rutina convierte el contenido de 'A' en ASCII
               ;a a hexadecimal, dejando el reltado en los 4 bits bajos de 'A'
               ;y llamando a continuacion a 'pack' con otro carcter ASCII en A
               ;vuelve con los dos en binario en 'a' el prim. el MSD

        sta sava
        clra
        sta savex
        lda sava
pack:
        cmp #$30        ;es menor que 30 ?
        bcs surt        ;si lo es sale con carry
        cmp #$47        ;es mayor que 47 ?
        bcc surt        ;si lo es sale con carry
        cmp #$3a        ;es menor que #10
        bcs pak1
        cmp #$40        ;es mayor que #10 ?
        bcs surt
        adc #8          ;suma 9 si el carry est  puesto
pak1:
        rola
        rola
        rola
        rola
        stx savex1      ;guarda 'x' para no machacar
        ldx #4
pak2:
        rola            ;pasa 'a' a savex
        rol savex       ;introduce el carry
        decx
        bne pak2
        ldx savex1      ;restaura 'x'lda savex
        lda savex
        clc
        rts

surt:
        sec
        rts

                        ;********************************


trans:

        lda #PUNFLE
        add contfle
        tax
        lda #!169
        sta bufdis,x            ;pone la flecha en la nueva posicion
        jsr refdis

        lda #PUNAMO
        add contfle
        tax
        decx
        lda bufdis,x            ;decenas de amplificadores
        jsr hex                 ;convierte a binario
        bcs nova
        incx                     ;va para las unidades
        lda bufdis,x
        jsr pack
        bcs nova
        sta buftran+2

        lda #PUNPAO
        add contfle
        tax
        decx
        lda bufdis,x            ;decenas de pantallas
        jsr hex                 ;convierte a binario
        bcs nova
        incx                     ;va para las unidades
        lda bufdis,x
        jsr pack
        bcs nova
        sta buftran+3
        clrx
tra:
        lda buftran,x
        jsr SciOut
        incx
        cpx #04
        bne tra
nova:
        rts

;****************************************************************************
                        ;esta rutina sirve para refrescar el display
refdis:
       jsr home
       clrx
refdi:
        lda bufdis,x
        jsr print               ;saca texto preparado en RAM
        incx
        cpx #84t
        bne refdi
        rts

pretim:
        lda TSR
        lda $19
        lda $18
        rts

;--------------------------------- subrutinas gestion display ----------------------------------


; Envia un caracter al display
; El display esta dividido en 8 páginas
;
; -------------------------------------
;| página 0        |    página 4       |
;| página 1        |    página 5       |
;| página 2        |    página 6       |
;| página 3        |    página 7       |
; -------------------------------------
;
;Las paginas 0 y 4 componen la linea 1 del display, las 1 y 5 la linea 2 y asi sucesivamente.
;En cada pagina caben 64 bytes, por lo que cuando se acaba de llenar una pagina, tenemos que
;conmutar a la siguiente pagina para poder completar la lina, es decir, si acabamos de llenar
;la pagina 0, tendremos que continuar por la 4, para ello nos ayudamos de la tabla <paginas>,
;donde nos indica la pagina donde tenemos que continuar y el ancho en bytes de la pagina.
;En las paginas 4,5,6 y 7, escribiremos 62 bytes, porque como los caracteres son de 6 bytes,
;nos quedarian partidos por la mitad al cambiar de linea.
;

print:
        stx svx
        cmp #!166
        bls n1
nn1     jmp tbl5

n1      cmp #!132
        bls n2
nn2     jmp tbl4

n2      cmp #!98
        bls n3
nn3     jmp tbl3

n3      cmp #!64
        bls n4
nn4     jmp tbl2

n4      cmp #!31
        bhs tbl
        jmp fprint

tbl     sub #$20
        ldx #$6
        fcb 42h                 ;mul
        tax
        clr tmp2
        jsr GetWidth            ;Miramos el ancho de la pagina
bc:
        lda tabla,x
        jsr DispChar
        bcc sgbc
        jmp fprint
sgbc    incx
        inc tmp2
        inc ncars
        lda ncars
        cmp PgWidth             ;Cada 64 bytes es una pagina
        bne BcCnt
        inc pag
        jsr StPage
BcCnt   lda tmp2

        cmp #$6
        bne bc
        jmp fprint


tbl2:
        sub #$41
        ldx #$6
        fcb 42h                         ;mul
        tax
        clr tmp2
        jsr GetWidth            ;Miramos el ancho de la pagina
bc2:
        lda tabla2,x
        jsr DispChar
        bcc no1
        jmp fprint
no1     incx
        inc tmp2
        inc ncars
        lda ncars
        cmp PgWidth             ;Cada 64 bytes es una pagina
        bne BcCnt2
        inc pag
        jsr StPage
BcCnt2  lda tmp2
        cmp #$6
        bne bc2
        jmp fprint


tbl3:
        sub #'c'
        ldx #$6
        fcb 42h                         ;mul
        tax
        clr tmp2
        jsr GetWidth            ;Miramos el ancho de la pagina
bc3:
        lda tabla3,x
        jsr DispChar
        bcc no2
        jmp fprint
no2     incx
        inc tmp2
        inc ncars
        lda ncars
        cmp PgWidth             ;Cada 64 bytes es una pagina
        bne BcCnt3
        inc pag
        jsr StPage
BcCnt3  lda tmp2
        cmp #$6
        bne bc3
        jmp fprint


tbl4:
        sub #!133
        ldx #$6
        fcb 42h                         ;mul
        tax
        clr tmp2
        jsr GetWidth            ;Miramos el ancho de la pagina
bc4:
        lda tabla4,x
        jsr DispChar
        bcc no3
        jmp fprint
no3     incx
        inc tmp2
        inc ncars
        lda ncars
        cmp PgWidth             ;Cada 64 bytes es una pagina
        bne BcCnt4
        inc pag
        jsr StPage
BcCnt4  lda tmp2
        cmp #$6
        bne bc4
        jmp fprint

tbl5:
        sub #!167
        ldx #$6
        fcb 42h                         ;mul
        tax
        clr tmp2
        jsr GetWidth            ;Miramos el ancho de la pagina
bc5:
        lda tabla5,x
        jsr DispChar
        bcc no4
        jmp fprint
no4     incx
        inc tmp2
        inc ncars
        lda ncars
        cmp PgWidth             ;Cada 64 bytes es una pagina
        bne BcCnt5
        inc pag
        jsr StPage
BcCnt5  lda tmp2
        cmp #$6
        bne bc5
        jmp fprint


; ANTES DE CAMBIAR DE PÁGINA, MIRAMOS SI LA HEMOS LLENADO TODA, EN EL CASO DE NO HABERLO HECHO
; ENVIAMOS CEROS HASTA EL FINAL DE LA PÁGINA.

StPage:
        lda ncars
        cmp #!64
        beq sgPage
        clra
        jsr DispChar
        inc ncars
        bra StPage

sgPage  stx savx2
        ldx pag
        lda #$2
        fcb 42h ; mul
        tax
        lda paginas,x
        jsr SetPage
        clr ncars
        clra
        jsr SetYPage            ;Ponemos la dirección Y a 0
        ldx savx2
        jsr GetWidth
        rts

GetWidth:
        stx savx2
        ldx pag
        lda #$2
        fcb 42h ;mul
        inca
        tax
        lda paginas,x
        sta PgWidth
        ldx savx2
        rts
fprint:
        ldx svx
        rts


;************* GENERA UN BACK SPACE ******************

BackSp:

        lda ncars
        cmp #0
        bne NoCars

        lda pag
        beq FinBack

        dec pag
        ldx pag
        lda #$2
        fcb 42h
        tax
        lda paginas,x
        jsr SetPage
        incx
        lda paginas,x
        deca
        sub #6
        sta ncars
        jsr SetYPage
        bra ncar

NoCars:
        lda ncars
        sub #6
        cmp #6
        bpl mmm
        clra
mmm     sta ncars
        jsr SetYPage            ;Ponemos la direcci¢n Y

ncar    lda #' '
        jsr print
        lda ncars
        sub #6
        sta ncars
        jsr SetYPage            ;Ponemos la direcci¢n Y
FinBack rts


;------------------------------------------------------------------------------

; Escribe un byte en el display
; El byte ha de venir en el acumulador

DispChar:
        sta sava
        jsr GetStatus                   ;Obtenemos el estado del display.
        brset ST_BUSY,flags,DispChar  ;Si está ocupado esperamos a que se libere.
        brset ST_RESET,flags,ErrChar  ;Si está en RESET, salimos con error.
        brset ST_ONOFF,flags,ErrChar  ;Si está desactivado sale con un error.

        lda #$ff
        sta DAT_DISP+4
        lda sava
        jsr invierte
        sta DAT_DISP

        bclr CS,CTRL_DISP
        nop
        nop
        nop
        nop
        nop
        bset E,CTRL_DISP
        nop
        nop
        nop
        nop
        nop
        bclr WR,CTRL_DISP
        nop
        nop
        nop
        nop
        nop
        bset DI,CTRL_DISP
        nop
        nop
        nop
        nop
        nop

        bclr E,CTRL_DISP
        nop
        nop
        nop
        nop
        nop
        bset CS,CTRL_DISP
        nop
        nop
        nop
        nop
        nop

        clc
        rts
ErrChar:
        sec
        rts

;******************************* BORRA LA PANTALLA ********************

cls:
        clra
        jsr SetPage
        clra
        jsr SetYPage

        clr ncars
        clr pag
BcCls:
        lda #' '
        jsr print
        lda pag
        cmp #$8
        bne BcCls

        clra
        jsr SetPage
        clra
        jsr SetYPage
        clr ncars
        clr pag
        rts
home:
        clra
        jsr SetPage             ;Ponemos la pagina 0 de RAM
        clra
        jsr SetYPage            ;Ponemos la dirección Y a 0
;        clra
;        jsr SetLine             ;Indicamos que la primerla lína a visualizar sera la 0
        clr pag
        clr ncars
        rts

;*********************  inicialización display ********************************
inidis:

        bset WR,CTRL_DISP+4
        nop
        nop
        nop
        nop
        nop
        bset DI,CTRL_DISP+4
        nop
        nop
        nop
        nop
        nop
        bset RST,CTRL_DISP+4
        nop
        nop
        nop
        nop
        nop
        bset CS,CTRL_DISP+4
        nop
        nop
        nop
        nop
        nop
        bset E,CTRL_DISP+4
        nop
        nop
        nop
        nop
        nop

        bset RST,CTRL_DISP      ;Activamos el RESET
        nop
        nop
        nop
        nop
        nop

        jsr DispOn              ;Ponemos el display en marcha
        jsr home
        rts


; Rutina para activar el display
DispOn:
        lda #$3f        ;Comando para activar el display
        jsr invierte
        jsr WrtCtrl
        rts

DispOff:
        lda #$3e        ;Comando para desactivar el display
        jsr invierte
        jsr WrtCtrl
        rts

; Pone la dirección x de la pagina RAM.
; El número de pagina ha de venir en el acumulador el rango es de 0-7

SetPage:
        cmp #$7
        bhi PgErr
        sta sava
        lda #$b8
        ora sava
        jsr invierte
        jsr WrtCtrl
PgErr:
        rts

; Pone la dirección y de la RAM
; El número de pagina ha de venir en el acumulador el rango es de 0-3f

SetYPage:
        sta sava
        lda #$40
        ora sava
        jsr invierte
        jsr WrtCtrl
        rts


; Pone la primera línea a mostrar en el display.
; El número de línea ha de estar en el acumulador, el rango es de 0-3f

SetLine:
        sta sava
        lda #$c0
        ora sava
        jsr invierte
        jsr WrtCtrl
        rts

;Rutina para enviar un comando de control al display
;El comando ha de estar en el acumulador

WrtCtrl:
        sta sava
wtst:
        jsr GetStatus                   ;Obtenemos el estado del display
        brset ST_BUSY,flags,wtst      ;Si está ocupado esperamos a que se libere
        brset ST_RESET,flags,ErrCmd   ;Si está en RESET, salimos con error

        lda #$ff
        sta DAT_DISP+4          ;Pone el puerto de datos como salidas
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop

        bclr WR,CTRL_DISP
        nop
        nop
        nop
        nop
        nop
        bclr DI,CTRL_DISP
        nop
        nop
        nop
        nop
        nop
        bclr CS,CTRL_DISP       ;Seleccionamos el DISPLAY
        nop
        nop
        nop
        nop
        nop
        bclr E,CTRL_DISP        ;Lo habilitamos
        nop
        nop
        nop
        nop
        nop

        lda sava
        sta DAT_DISP

        bset E,CTRL_DISP
        nop
        nop
        nop
        nop
        nop
        bset CS,CTRL_DISP
        nop
        nop
        nop
        nop
        nop

        clc
        rts
ErrCmd:
        bset E,CTRL_DISP        ;Lo habilitamos
        nop
        nop
        nop
        nop
        nop
        bset CS,CTRL_DISP
        nop
        nop
        nop
        nop
        nop
        sec
        rts


;Miramos el estado del display
;En el flags está el estado del display

GetStatus:

        clr DAT_DISP+4          ;Ponemos el puerto de datos como entradas
        bclr DI,CTRL_DISP
        nop
        nop
        nop
        nop
        nop
        bset WR,CTRL_DISP
        nop
        nop
        nop
        nop
        nop
        bclr CS,CTRL_DISP
        nop
        nop
        nop
        nop
        nop
        bclr E,CTRL_DISP
        nop
        nop
        nop
        nop
        nop
        bset E,CTRL_DISP
        nop
        nop
        nop
        nop
        nop

        lda DAT_DISP
        and #$e0
        ora flags
        sta flags
        bset CS,CTRL_DISP
        rts



invierte:

        stx savx
        rola
        rorx
        rola
        rorx
        rola
        rorx
        rola
        rorx
        rola
        rorx
        rola
        rorx
        rola
        rorx
        rola
        rorx
        txa
        ldx savx
        rts
;-----------------------------------------------------------------------------

; Configura el puerto SCI
; En A ha de estar el valor del registro SCCR1.
; En X ha de estar el valor del registro BAUD. (velocidad)


SciInit:

        sta SCCR1
        LDA #$0C        ;- TDRE interrupt disabled
                        ;- TC interrupt disabled
                        ;- SCI interrupt disabled
                        ;- IDLE interrupt disbaled
                        ;- After the last byte is transmitted,the TDO line
                        ;  becomes a hight-impedance line.
                        ;- Receiver disabled and RDRF,IDLE,OR,NF and FE
                        ;  status bits are inhibited.
        sta SCCR2
        stx BAUD
        rts

;;;***************** DESACTIVA EL MODO SCI *******************************
SciOff:
        lda #$00
        sta SCCR1       ;one start bit,eight data bits, one stop bit

        lda #$00        ;- TDRE interrupt disabled
                        ;- TC interrupt disabled
                        ;- SCI interrupt enabled
                        ;- IDLE interrupt disbaled
                        ;- Transmiter enabled
                        ;- Receiver enabled
                        ;  status bits are inhibited.
        sta SCCR2
        rts



;-------------- TRANSMITE AL PC EL CONTENIDO DEL REGISTRO A ------------------

SciOut:
        brclr 7,SCSR,*        ;MIRA SI ESTA OCUPADO
        sta SCDAT
        brclr 6,SCSR,*             ;ESPERA PARA ACABAR DE TRANSMITIR
        rts

;-----------------------------------------------------------------------------

SciIn:
        brclr 5,SCSR,*               ;ESPERA QUE ENTREN UN CARACTER
        lda SCDAT                    ;EN A ESTA EL CARACTER ENTRADO
        rts




;-------------------------------- ROM DATA -----------------------------------

        ; Tabla de caracteres

tabla:
        fcb $00,$00,$00,$00,$00,$00     ; ESPACIO
        fcb $00,$06,$5f,$06,$00,$00     ; !
        fcb $07,$03,$00,$07,$03,$00     ; "
        fcb $24,$7e,$24,$7e,$24,$00     ; #
        fcb $00,$24,$2b,$6a,$12,$00     ; $
        fcb $63,$13,$08,$64,$63,$00     ; %
        fcb $00,$00,$00,$00,$00,$00     ; &
        fcb $00,$00,$07,$03,$00,$00     ; '
        fcb $00,$00,$3e,$41,$00,$00     ; (
        fcb $00,$00,$41,$3e,$00,$00     ; )
        fcb $04,$1f,$0e,$1f,$04,$00     ; ;
        fcb $08,$08,$3e,$08,$08,$00     ; +
        fcb $00,$00,$00,$00,$00,$00     ; ´
        fcb $08,$08,$08,$08,$08,$00     ; -
        fcb $00,$00,$60,$60,$00,$00     ; .
        fcb $20,$10,$08,$04,$02,$00     ; /
        fcb $3e,$51,$49,$45,$3e,$00     ; 0
        fcb $00,$42,$7f,$40,$00,$00     ; 1
        fcb $62,$51,$49,$49,$46,$00     ; 2
        fcb $22,$49,$49,$49,$36,$00     ; 3
        fcb $18,$14,$12,$7f,$10,$00     ; 4
        fcb $2f,$49,$49,$49,$31,$00     ; 5
        fcb $3c,$4a,$49,$49,$30,$00     ; 6
        fcb $01,$71,$09,$05,$03,$00     ; 7
        fcb $36,$49,$49,$49,$36,$00     ; 8
        fcb $06,$49,$49,$29,$1e,$00     ; 9
        fcb $00,$00,$6c,$6c,$00,$00     ; :
        fcb $00,$00,$76,$36,$00,$00     ; ;
        fcb $00,$08,$14,$22,$41,$00     ; <
        fcb $24,$24,$24,$24,$24,$00     ; =
        fcb $00,$41,$22,$14,$08,$00     ; >
        fcb $02,$01,$59,$09,$06,$00     ; ?
        fcb $3e,$41,$5d,$55,$1e,$00     ; @
tabla2:
        fcb $7e,$11,$11,$11,$7e,$00     ; A
        fcb $7f,$49,$49,$49,$36,$00     ; B
        fcb $3E,$41,$41,$41,$22,$00     ; C
        fcb $7f,$41,$41,$41,$3e,$00     ; D
        fcb $7f,$49,$49,$49,$41,$00     ; E
        fcb $7f,$09,$09,$09,$01,$00     ; F
        fcb $3e,$41,$49,$49,$7a,$00     ; G
        fcb $7f,$08,$08,$08,$7f,$00     ; H
        fcb $41,$7f,$41,$00,$00,$00     ; I
        fcb $30,$40,$40,$40,$3f,$00     ; J
        fcb $7f,$08,$14,$22,$41,$00     ; K
        fcb $7f,$40,$40,$40,$40,$00     ; L
        fcb $7f,$02,$04,$02,$7f,$00     ; M
        fcb $7f,$02,$04,$08,$7f,$00     ; N
        fcb $3e,$41,$41,$41,$3e,$00     ; O
        fcb $7f,$09,$09,$09,$06,$00     ; P
        fcb $3e,$41,$61,$21,$5e,$00     ; Q
        fcb $7f,$09,$09,$19,$66,$00     ; R
        fcb $26,$49,$49,$49,$32,$00     ; S
        fcb $01,$01,$7f,$01,$01,$00     ; T
        fcb $7f,$40,$40,$40,$7f,$00     ; U
        fcb $1f,$20,$40,$20,$1f,$00     ; V
        fcb $7f,$40,$3c,$40,$7f,$00     ; W
        fcb $63,$14,$08,$14,$63,$00     ; X
        fcb $07,$08,$70,$08,$07,$00     ; Y
        fcb $71,$49,$45,$43,$00,$00     ; Z
        fcb $7f,$41,$41,$00,$00,$00     ; [
        fcb $01,$02,$04,$08,$10,$00     ; \
        fcb $41,$41,$7f,$00,$00,$00     ; ]
        fcb $04,$02,$01,$02,$04,$00     ; ^
        fcb $80,$80,$80,$80,$80,$80     ; _
        fcb $03,$07,$00,$00,$00,$00     ; `
        fcb $20,$54,$54,$54,$78,$00     ; a
        fcb $7f,$44,$44,$44,$38,$00     ; b
tabla3:
        fcb $38,$44,$44,$44,$28,$00     ; c
        fcb $38,$44,$44,$44,$7f,$00     ; d
        fcb $38,$54,$54,$54,$08,$00     ; e
        fcb $08,$fe,$09,$09,$00,$00     ; f
        fcb $18,$a4,$a4,$a4,$7c,$00     ; g
        fcb $7f,$04,$04,$78,$00,$00     ; h
        fcb $00,$00,$7d,$40,$00,$00     ; i *
        fcb $04,$08,$84,$7d,$00,$00     ; j
        fcb $7f,$10,$28,$44,$00,$00     ; k
        fcb $00,$00,$7F,$40,$00,$00     ; l *
        fcb $7c,$04,$18,$04,$78,$00     ; m *
        fcb $00,$7c,$04,$04,$78,$00     ; n
        fcb $38,$44,$44,$44,$38,$00     ; o
        fcb $fc,$44,$44,$44,$38,$00     ; p
        fcb $38,$44,$44,$44,$fc,$00     ; q
        fcb $44,$78,$44,$04,$08,$00     ; r *
        fcb $08,$54,$54,$54,$20,$00     ; s
        fcb $04,$3e,$44,$24,$00,$00     ; t *
        fcb $3c,$40,$20,$7c,$00,$00     ; u
        fcb $1c,$20,$40,$20,$1c,$00     ; v
        fcb $3c,$60,$30,$60,$3c,$00     ; w
        fcb $6c,$10,$10,$6c,$00,$00     ; x
        fcb $9c,$a0,$60,$9c,$00,$00     ; y
        fcb $64,$58,$58,$4c,$00,$00     ; z
        fcb $08,$3e,$41,$41,$00,$00     ; {     ALT- 0123
        fcb $00,$00,$77,$00,$00,$00     ; |     ALT- 0124
        fcb $41,$41,$3e,$08,$00,$00     ; }     ALT- 0125
        fcb $02,$01,$02,$01,$00,$00     ; ~     ALT- 0126
        fcb $3c,$26,$23,$26,$3c,$00     ;      ALT- 0127
        fcb $17,$a1,$e1,$21,$12,$00     ; €     ALT- 0128
        fcb $3d,$40,$20,$3d,$00,$00     ;      ALT- 0129
        fcb $38,$54,$54,$55,$09,$00     ; ‚     ALT- 0130
        fcb $20,$55,$55,$55,$78,$00     ; ƒ     ALT- 0131
        fcb $20,$55,$54,$55,$78,$00     ; „     ALT- 0132
tabla4:
        fcb $20,$55,$55,$54,$78,$00     ; …     ALT- 0133
        fcb $20,$57,$55,$57,$78,$00     ; †     ALT- 0134
        fcb $1c,$a2,$e2,$a2,$14,$00     ; ‡     ALT- 0135
        fcb $38,$55,$55,$55,$08,$00     ; ˆ     ALT- 0136
        fcb $38,$55,$54,$55,$97,$00     ; ‰     ALT- 0137
        fcb $38,$55,$55,$54,$97,$00     ; Š     ALT- 0138
        fcb $00,$01,$7c,$41,$00,$00     ; ‹     ALT- 0139
        fcb $00,$02,$79,$42,$00,$00     ; Œ     ALT- 0140
        fcb $00,$01,$7c,$20,$00,$00     ;      ALT- 0141
        fcb $70,$29,$24,$29,$70,$00     ; Ž     ALT- 0142
        fcb $78,$2f,$25,$2f,$78,$00     ;      ALT- 0143
        fcb $7c,$54,$54,$55,$45,$00     ;      ALT- 0144
        fcb $34,$54,$7c,$54,$58,$00     ; ‘     ALT- 0145
        fcb $7e,$09,$7f,$49,$49,$00     ; ’     ALT- 0146
        fcb $38,$45,$45,$39,$00,$00     ; “     ALT- 0147
        fcb $38,$45,$44,$39,$00,$00     ; ”     ALT- 0148
        fcb $39,$45,$44,$39,$00,$00     ; •     ALT- 0149
        fcb $3c,$41,$21,$7e,$00,$00     ; –     ALT- 0150
        fcb $3d,$41,$20,$7c,$00,$00     ; —     ALT- 0151
        fcb $9c,$a1,$60,$3d,$00,$00     ; ˜     ALT- 0152
        fcb $3d,$42,$42,$3D,$00,$00     ; ™     ALT- 0153
        fcb $3c,$41,$40,$3d,$00,$00     ; š     ALT- 0154
        fcb $18,$24,$66,$24,$00,$00     ; ›     ALT- 0155
        fcb $48,$3e,$49,$49,$62,$00     ; œ     ALT- 0156
        fcb $29,$2a,$7c,$2a,$29,$00     ;      ALT- 0157
        fcb $7f,$09,$16,$78,$00,$00     ; ž     ALT- 0158
        fcb $40,$88,$7e,$09,$02,$00     ; Ÿ     ALT- 0159
        fcb $20,$54,$55,$55,$78,$00     ;       ALT- 0160
        fcb $00,$00,$7d,$41,$00,$00     ; ¡     ALT- 0161
        fcb $00,$38,$44,$46,$39,$00     ; ¢     ALT- 0162
        fcb $3c,$40,$21,$7d,$00,$00     ; £     ALT- 0163
        fcb $7a,$09,$0a,$71,$00,$00     ; ¤     ALT- 0164
        fcb $7a,$11,$22,$79,$00,$00     ; ¥     ALT- 0165
        fcb $08,$55,$55,$55,$5e,$00     ; ¦     ALT- 0166
tabla5:
        fcb $4e,$51,$51,$4e,$00,$00     ; §     ALT - 167
        fcb $30,$48,$4d,$40,$20,$00     ; ¨     ALT - 168
        fcb $04,$06,$7f,$06,$04,$00     ; FLECHA ARRIBA         ALT - 169
        fcb $10,$30,$7f,$30,$10,$00     ; FLECHA ABAJO          ALT - 170
        fcb $00,$20,$70,$a8,$20,$37     ; DIBUJO DE INTRO       ALT - 171

paginas:
        fcb 0t,64t
        fcb 4t,62t
        fcb 1t,64t
        fcb 5t,62t
        fcb 2t,64t
        fcb 6t,62t
        fcb 3t,64t
        fcb 7t,62t
                      END


;***************************** R O M  D A T A ****************************
texto:
        fcb '   **** COMPAX ****  '
        fcb '                     '
        fcb 'Electr'
        fcb !162                       ;¢
        fcb 'nica Barcelona'
        fcb 00h
texto1:
        fcb 'Amp>-                '
        fcb 'Pan>-                '
        fcb 'Fun. '
        fcb !169
        fcb '               '
        fcb 'Amplif.--  Pantall.--'
        fcb 00h
tabla6:
        fcb 11h,'0',21h,'C',41h,'E',81h,0Dh,12h,'8',22h,'7',42h,'9'
        fcb 82h,'D',14h,'5'
        fcb 24h,'4',44h,'6',84h,'B',18h,'2',28h,'1',48h,'3',88h,'A'


;**************************** RUTINAS INTERRUPION ***************************

timer:
        brclr 4,flags,saltim
        dec contim      ;contador para hacer bucles
        bne saltim
        bclr 4,flags
saltim:
        jsr pretim
        rti

;******************************************************************************


IRQ:
SPI:
SCI:
SWI:

        RTI
;******************************************************************************

;************************ VECTORES INTERRUPCION *******************************
                               org 1FF4H

        fdb SPI
        fdb SCI
        fdb timer
        fdb IRQ
        fdb SWI
        fdb ENTRY

;******************************************************************************

;******************** CONFIGURACION REGISTROS ********************************
*                               org 1ff0h

*        fdb 0

;*****************************************************************************

        END

