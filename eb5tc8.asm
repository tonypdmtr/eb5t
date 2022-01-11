*-------------------------------------------------------------------------------
*NOMBRE : CENDEC5T.SRC
*DESCRIP: Codificador/decodificador tonos ZVI
*MICRO. : MC68HC705J1A
*OBSERV.: - Controla el fx803 por el SPI.
*         - Con filtro.
*
*-------------------------------------------------------------------------------
                OPT MUL

* DEFINICION DE LOS PORTS
PORTA   equ 00h
PORTB   equ 01h
PORTC   equ 02h
PORTD   equ 03h
DDRA    equ 04h
DDRB    equ 05h
DDRC    equ 06h
mr      equ 0ah                 *miscellaneous register

GENERAL_RESET           equ 01h
WRITE_CONTROL_REG       equ 30h
READ_STATUS_REG         equ 31h
READ_RX_TONE            equ 32h
WR_NOTONE               equ 33h
WR_GEN1              equ 34h
WR_GEN2              equ 35h
WRITE_GP_TIMER          equ 36h

AUDIO_SWITCH            equ 7
GP_TIMER_INT            equ 6
DEC_INT		        equ 5
SUMMING_SWITCH          equ 4
HIGH_BAND               equ 00
MID_BAND                equ 04
EXTENDED_BAND           equ 08

CS        equ 2
SCK       equ 5
RDATA     equ 4
CD        equ 3

FX803	  equ 1

BT_PTT    equ 1

                       bsct
                       dsct

flag      rmb 1

sava      rmb 1
savx      rmb 1
temp0     rmb 1
temp      rmb 1
Nh        rmb 1
RhH       rmb 1
RhL       rmb 1
result0   rmb 1
result1   rmb 1
result2   rmb 1
result3   rmb 1
mltndo0   rmb 1
mltndo1   rmb 1
mltdor    rmb 1
r0        rmb 1
r1        rmb 1
r3        rmb 1
temp1     rmb 1
temp2     rmb 1
dvsor1    rmb 1
dvsor2    rmb 1
temp3     rmb 1
temp4     rmb 1
temp5     rmb 1
dvsor0    rmb 1
temp6     rmb 1
CtrlReg   rmb 1

CntByte   rmb 1
carr      rmb 1
result    rmb 6
PntResta  rmb 1
ResRes	  rmb 6
string    rmb 10
AntChar   rmb 1

                           psct
reset:

        clrx

        clr DDRA

        lda #$2e
        sta DDRB

        clr PORTA
        clr PORTB

        clrx
bc:
        clra
        sta $50,x               *Inicializa la memoria con 00
        incx
        cpx #$40
        bne bc

        bset CS,FX803
        sei
        jsr ini803

main:
        bil ptt

        jsr RdStatus            *MIRA EL ESTADO DEL 803, EL STATUS ESTA EN TEMP
        brset 0,temp,TonVal
        brset 1,temp,not
*        brset 2,temp,GpTimer

        bra main

TonVal:
	*HA RECIBIDO UN TONO VALIDO.

        bclr CS,FX803
        lda #READ_RX_TONE
        jsr SpiOut
        jsr SpiIn
        sta Nh
        jsr SpiIn
        sta RhL
        bset CS,FX803
        jsr formula
bs:
        clrx
bsc:
        lda eea_rx,x
        beq NoExis      *ESTE TONO NO EXISTE EN LA TABLA.

        cmp r1
        beq EsNum
        lda r1
        sub #$2
        cmp eea_rx,x
        bcc sbsc
        add #$4
        cmp eea_rx,x
        bpl EsNum
sbsc    incx
        bra bsc

NoExis:
        bra main

EsNum:
        lda eea_rx,x
        bra main
not:

        *PROGRAMA EL NOTONE REGISTER CON 40 ms

        bclr CS,FX803    *chip select
        lda #WR_NOTONE   *direccion no tone register
        jsr SpiOut
        lda #$02
        jsr SpiOut      *tiempo maximo
        bset CS,FX803
        jmp main

ptt:
        jsr RqBih
        bcc main

        bset BT_PTT,PORTB       *Pone PTT
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle

        bclr CS,FX803
        lda #WRITE_CONTROL_REG
        jsr SpiOut
        bclr GP_TIMER_INT,CtrlReg
        bclr AUDIO_SWITCH,CtrlReg
        bset SUMMING_SWITCH,CtrlReg
        bclr DEC_INT,CtrlReg    *No permite interrupts del decodificador
        lda CtrlReg
        jsr SpiOut
        bset CS,FX803

        bclr CS,FX803    *chip select
        lda #WR_NOTONE   *direccion no tone register
        jsr SpiOut
        lda #$00
        jsr SpiOut
        bset CS,FX803

        bclr CS,FX803
        lda #WRITE_GP_TIMER
        jsr SpiOut
                         *04 para 40 ms
                         *07 para 70 ms
        lda #$07         *Programa ms para timer
        jsr SpiOut
        bset CS,FX803
wt0:
        jsr RdStatus
        brclr 2,temp,wt0

        lda PORTB
        and #$01
        sta result+4
        lda PORTA
        sta result+5

        clc
        lda result+5
        inca
        sta result+5
        lda result+4
        adc #$0
        sta result+4

        jsr Hex2Asc

        clr AntChar
        ldx #$5

*---------------- cabecera para ¨? -------------

        lda #'0'
        sta string,x
        incx
        lda #'0'
        sta string,x

*-----------------------------------------------

        ldx #$5

snd:
        lda string,x
        cmp AntChar
        bne NoIgual

        lda #$3b
        sta AntChar
        bra SiIgual

NoIgual sta AntChar
SiIgual sub #$30
        stx savx
        sta sava

        lda sava
        ldx #2
        mul
        tax

        lda zvi_tx,x
        sta temp
        incx
        lda zvi_tx,x
        sta temp1

        bclr CS,FX803
        lda #WR_GEN1
        jsr SpiOut
        lda temp
        jsr SpiOut
        lda temp1
        jsr SpiOut
        bset CS,FX803

wt1:
        jsr RdStatus
        brclr 2,temp,wt1

        ldx savx
        incx
        cpx #$a
        bne snd

* DESACTIVA EL TRANSMISOR

        bclr CS,FX803    *chip select
        lda #WR_GEN1
        jsr SpiOut
        lda #$20
        jsr SpiOut
        lda #$00
        jsr SpiOut
        bset CS,FX803    *chip select

        bclr CS,FX803    *chip select
        lda #WR_GEN2
        jsr SpiOut
        lda #20
        jsr SpiOut
        lda #00
        jsr SpiOut
        bset CS,FX803    *chip select

        bclr CS,FX803
        lda #WRITE_CONTROL_REG
        jsr SpiOut
        bclr GP_TIMER_INT,CtrlReg
        bset AUDIO_SWITCH,CtrlReg
        bclr SUMMING_SWITCH,CtrlReg
        bclr DEC_INT,CtrlReg    *No permite interrupts del decodificador

        lda CtrlReg
        jsr SpiOut
        bset CS,FX803

        bclr BT_PTT,PORTB       *Quita el PTT

wt2     jsr RqBih
        bcs wt2

        jsr ini803
        jmp main

RqBih:
        lda #$10
rqb1    bih rqb2
        deca
        bne rqb1
        sec
        rts

rqb2    lda #$10
rqb3   bil RqBih
        deca
        bne rqb3
        clc
        rts

bucle:
        ldx #$3f
bu0     lda #$ff
bu1     nop
        nop
        nop
        deca
        bne bu1
        decx
        bne bu0
        rts

*------------------------------ calcula la resta 511-rh ------------------------
formula:

*el numero a restar ha de venir en RhL

        lda #$ff
        sub RhL
        sta RhL
        lda #$1
        sta RhH

*resultado =
*       RhH byte alto (siempre 1)
*       RhL byte bajo

*-------------------------------------------------------------------------------

*-------------------------------- multiplica 28000 x Nh ------------------------

        lda #$6d
        sta mltndo0
        lda #$60
        sta mltndo1             *6d60h = 28000d
        lda Nh
        sta mltdor
        jsr multi               *multiplico 28000 x Nh

*-------------------------------------------------------------------------------

*------------------- divide (28000xNh) / (511-rh) ------------------------------

        lda result2             *el resultado queda en result0, result1, result2
        sta r3
        lda result1
        sta r0
        lda result0
        sta r1

        lda #$00
        sta dvsor0
        lda RhH
        sta dvsor1
        lda RhL
        sta dvsor2
        jsr div

        lda r3
        and #$f0
        rora
        rora
        rora
        rora
        sta temp

        lda r0
        rola
        rola
        rola
        rola
        ora temp
        sta r0
        lda r3
        and #$0f        *multiplica por 10 el resultado de la formula para dar
        sta r3          *mas precision.
        rts

* ---------------------------------------- div ---------------------------------
*subrutina para dividir.
*en dvsor1 (byte alto) y divsor2 (bajo) va el divisor
*en r1 (byte alto) y r0 (bajo) el dividendo
*el resultado queda en r1 y r0

div:
        com dvsor0
        com dvsor1
        com dvsor2      *complementa divisor
        clc
        lda #$1         *increm.divisor para dos complem.
        adc dvsor2
        sta dvsor2
        clra
        adc dvsor1
        sta dvsor1
        clra
        adc dvsor0
        sta dvsor0
        clr temp0
        clr temp1       *valor inicial para resto
        clr temp2
        ldx #25         *contador
dv0:
        clc             *suma dvsor1-2 a temp1-2
        lda temp2
        adc dvsor2
        sta temp4
*
        lda temp1
        adc dvsor1
        sta temp3
        lda temp0
        adc dvsor0
        sta temp5
*
        bcc dv1
        lda temp4
        sta temp6
        lda temp2
        sta temp4
        lda temp6
        sta temp2
        lda temp3
        sta temp6
        lda temp1
        sta temp3
        lda temp6
        sta temp1
        lda temp5
        sta temp6
        lda temp0
        sta temp5
        lda temp6
        sta temp0

dv1:
        rol r3
        rol r0          *rota byte bajo dividendo
        rol r1
        rol temp2
        rol temp1
        rol temp0
        decx
        bne dv0
*                       tras la division entera, rota resto y lo deja en temp1-2
        ror temp0
        ror temp1
        ror temp2
        rts
*
*-------------------------------------------------------------------------------

*------------------------------------------ multi-------------------------------
* en mltndo0 byte alto multiplicador
* en mltndo1 byte bajo multiplicador
* en mltdor multiplicando.
* result0, result1, result2 resultado

multi:
        clr result0
        clr result1
        clr result2

        lda mltndo1
        ldx mltdor

        mul

        stx result1
        sta result2     *resultado byte bajo final

        stx sava

        lda mltndo0
        ldx mltdor
        mul
        sta result1
        clc
        lda sava
        add result1
        bcs haycarry

        sta result1
        stx result0
        rts

haycarry:

        inc result0
        ldx mltdor
        mul
        sta result1

        txa
        add result0
        ldx mltdor
        mul

        sta result0
        rts
*-------------------------------------------------------------------------------

*------------------------------------ subrutinas gestion 803 -------------------

*- - - - - - - - - - - - - - - - - - - - inicializa el 803 - - - - - - - - - - -
ini803:

*primero hace un reset del 803
*

        bclr CS,FX803
        lda #GENERAL_RESET
        jsr SpiOut
        bset CS,FX803

*transmite un 00 al control register
*
*  bits                 significado
*
*   7                audi switch          0 = disable
*   6                g/purpose timer      0 = disable
*   5                decoder interrupts   0 = disable
*   4                summing switch       1 = enable
*   3-2              band selection     0 0 = high band
*   1-0              no se usa          0 0
*
        bclr CS,FX803    *chip select
        lda #WRITE_CONTROL_REG      *direccion control register
        jsr SpiOut
        lda #$80                    *instruccion control register
        sta CtrlReg
        jsr SpiOut
        bset CS,FX803

* desactiva el transmisor

        bclr CS,FX803    *chip select
        lda #WR_GEN1
        jsr SpiOut
        lda #$20
        jsr SpiOut
        lda #$00
        jsr SpiOut
        bset CS,FX803    *chip select

        bclr CS,FX803    *chip select
        lda #WR_GEN2
        jsr SpiOut
        lda #20
        jsr SpiOut
        lda #00
        jsr SpiOut
        bset CS,FX803    *chip select

*programa el notone register con 20 ms

        bclr CS,FX803    *chip select
        lda #WR_NOTONE   *direccion no tone register
        jsr SpiOut
        lda #$02
        jsr SpiOut      *tiempo maximo
        bset CS,FX803

        bclr CS,FX803    *chip select
        lda #WRITE_CONTROL_REG
        jsr SpiOut
        bclr DEC_INT,CtrlReg    *no permite interrupts del decodificador
        lda CtrlReg
        jsr SpiOut
        bset CS,FX803
        rts

*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*- - - - - - - - TRANSMITE AL SPI EL CONTENIDO DE A - - - - - - - - - - - - -

SpiOut:

        ldx #$8
        clc

loop    rola
        bcs uno

        bclr CD,FX803
clk     bclr SCK,FX803
        bset SCK,FX803
        decx
        cpx #$00
        bne loop
        rts

uno     bset CD,FX803
        bra clk

*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RdStatus:

        bclr CS,FX803
        lda #$31
        jsr SpiOut
        jsr SpiIn
        sta temp
        bset CS,FX803
        rts

*- - - - - - - - - LEE UN BYTE DEL SPI Y LO PONE EN A - - - - - - - - - - - -
SpiIn:

        ldx #$8
        clc
        clra

loo2    bclr SCK,FX803
        bset SCK,FX803
        brset RDATA,FX803,EsUno
        clc

clk2    rola
        decx
        cpx #$00
        bne loo2
        rts

EsUno   sec
        bra clk2
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*//-------------- Convierte un n£mero binario a ascii decimal -----------------
*// Convierte un n£mero binario a una cadena de car cteres ascii
*// El n£mero ha de estar en result
*// La cadena la pone en string

Hex2Asc:
        clr sava
        clr savx
        clr PntResta
BucHex
        jsr resta
        bcs HayC

        lda ResRes
        sta result
        lda ResRes+1
        sta result+1
        lda ResRes+2
        sta result+2
        lda ResRes+3
        sta result+3
        lda ResRes+4
        sta result+4
        lda ResRes+5
        sta result+5
        inc sava

BucH:
        lda PntResta
        cmp #$32
        bne BucHex
        rts

HayC:

        ldx savx
        lda sava
        add #'0'
        sta string,x
        inc savx
        clr sava
        lda PntResta
        add #$5
        sta PntResta
        bra BucH
*----------------------------------- RESTA -----------------------------------
resta:

        clr carr
        lda #$5
        sta CntByte
        lda PntResta
        add #$4
        sta PntResta
        tax

restB:
        ldx PntResta
        dec PntResta
        lda const,x
        add carr
        sta temp
        clr carr

        ldx CntByte
        lda result,x
        sub temp
        bcc NoCarry

        inc carr

NoCarry:
        ldx CntByte
        sta ResRes,x
        dec CntByte
        lda CntByte
        bne restB
        inc PntResta
        clc
        lda carr
        beq FinRes
        sec
FinRes:
        rts

*-----------------------------------------------------------------------------

*=============================================================================

*=========================  TABLAS Y MENSAJES ================================

const:

         fcb 0,3bh,9ah,0cah,0
         fcb 0,5h,0f5h,0e1h,0
         fcb 0,0,98h,96h,80h
         fcb 0,0,0fh,42h,40h
         fcb 0,0,1,86h,0a0h
         fcb 0,0,00h,27h,10h
         fcb 0,0,00h,3h,0e8h
         fcb 0,0,00h,00h,64h
         fcb 0,0,00h,00h,0ah
         fcb 0,0,00h,00h,01h

eea_tx:

        fcb 001h,0fch	* '0'
        fcb 003h,080h	*  '1'
        fcb 003h,04ah	*  '2'
        fcb 003h,016h	*  '3'
        fcb 002h,0e6h	*  '4'
        fcb 002h,0b9h	*  '5'
        fcb 002h,08eh	*  '6'
        fcb 002h,066h	*  '7'
        fcb 002h,040h	*  '8'
        fcb 002h,01dh	*  '9'
        fcb 003h,0bbh	*  'G'
        fcb 001h,0ddh	*  'R'

eea_rx:

        fcb 07bh
        fcb 046h
        fcb 04ah
        fcb 04fh
        fcb 054h
        fcb 05ah
        fcb 060h
        fcb 066h
        fcb 06ch
        fcb 074h
        fcb 083h
        fcb 00

zvi_rx:

        fcb 095h
        fcb 042h
        fcb 048h
        fcb 04fh
        fcb 057h
        fcb 05fh
        fcb 068h
        fcb 072h
        fcb 07ch
        fcb 089h
        fcb 0a2h
        fcb 00

zvi_tx:
        fcb 01h,0a4h    *0
        fcb 03h,0b6h    *1
        fcb 03h,064h    *2
        fcb 03h,019h    *3
        fcb 02h,0d0h    *4
        fcb 02h,092h    *5
        fcb 02h,05bh    *6
        fcb 02h,026h    *7
        fcb 01h,0f8h    *8
        fcb 01h,0cah    *9
        fcb 00h,00h     *G
        fcb 01h,083h    *R

irq:
spi:
sci:
timer:
swi:
        rti

         asct

************************* VECTORES INTERRUPCION *******************************
         org   1FF4H

         fdb   spi
         fdb   sci
         fdb   timer
         fdb   irq
         fdb   swi
         fdb   reset

*******************************************************************************

********************* CONFIGURACION REGISTROS ********************************
         org   1ff0h

         fdb   0000

         ORG   1FDFH    *OPTION REGISTER

         fcb   0C2H     *RAM0 * 1
                        *RAM1 * 1
****************************************************************************
        end
