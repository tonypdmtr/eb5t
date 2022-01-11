;-------------------------------------------------------------------------------
;NOMBRE : CENDEC5T.SRC
;DESCRIP: Codificador/decodificador tonos ZVI
;MICRO. : MC68HC705J1A
;OBSERV.: - Controla el FX803 por el SPI.
;         - Con filtro.
;
;-------------------------------------------------------------------------------

GENERAL_RESET           equ 01h
WRITE_CONTROL_REG       equ 30h
READ_STATUS_REG         equ 31h
READ_RX_TONE            equ 32h
WRITE_NOTONE_TIMER      equ 33h
WRITE_GEN1              equ 34h
WRITE_GEN2              equ 35h
WRITE_GP_TIMER          equ 36h

AUDIO_SWITCH            equ 7
GP_TIMER_INT            equ 6
DECODER_INT             equ 5
SUMMING_SWITCH          equ 4
HIGH_BAND               equ 00
MID_BAND                equ 04
EXTENDED_BAND           equ 08

CS        equ 2
SCK       equ 5
RDATA     equ 4
CD        equ 3

FX803     equ 1

BT_PTT    equ 1

                org RAM

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
resultRes rmb 6
string    rmb 10
AntChar   rmb 1

                           org ROM
                           jmp start
indic   fcb 2,5
tiempo  fcb 2

start:

        clrx

        clr DDRA

        lda #$2e
        sta DDRB

        clr PORTA
        clr PORTB

        clrx
bc:
        clra
        sta RAM,x               ;Inicializa la memoria con 00
        incx
        cpx #$40
        bne bc

        bset CS,FX803
        sei
        jsr Ini803

main:
        bil ptt

        jsr RdStatus            ;MIRA EL ESTADO DEL 803, EL STATUS ESTA EN TEMP
        brset 0,temp,tonVal
        brset 1,temp,not
;        brset 2,temp,GpTimer

        bra main

TonVal:                         ;HA RECIBIDO UN TONO VALIDO.

        bclr CS,FX803
        lda #READ_RX_TONE
        jsr SpiOut
        jsr SpiIn
        sta Nh
        jsr SpiIn
        sta Rhl
        bset CS,FX803
        jsr formula
bs:
        clrx
bsc:
        lda eea_rx,x
        beq NoExis      ;ESTE TONO NO EXISTE EN LA TABLA.

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

        ;PROGRAMA EL NOTONE REGISTER CON 40 ms

        BCLR CS,FX803    ;CHIP SELECT
        LDA #WRITE_NOTONE_TIMER     ;DIRECCION NO TONE REGISTER
        JSR SPIOUT
        LDA tiempo
        JSR SPIOUT      ;TIEMPO MAXIMO
        BSET CS,FX803
        jmp main

ptt:
        jsr RqBih
        bcc main

        bset BT_PTT,PORTB       ;Pone PTT
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
        bclr DECODER_INT,CtrlReg    ;No permite interrupts del decodificador
        lda CtrlReg
        jsr SpiOut
        bset CS,FX803

        BCLR CS,FX803    ;CHIP SELECT
        LDA #WRITE_NOTONE_TIMER     ;DIRECCION NO TONE REGISTER
        JSR SPIOUT
        LDA #$00
        JSR SPIOUT
        BSET CS,FX803

        bclr CS,FX803
        lda #WRITE_GP_TIMER
        jsr SpiOut
                         ;02 para 20 ms
                         ;04 para 40 ms
                         ;07 para 70 ms
        lda tiempo       ;Programa ms para timer
        jsr SpiOut
        bset CS,FX803
Wt0:
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

; cabecera para SERVI TAXI '25'

        lda indic
        add #$30
        sta string,x
        incx
        lda indic+1
        add #$30
        sta string,x

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

        lda ZVI_TX,x
        sta temp
        incx
        lda ZVI_TX,x
        sta temp1

        bclr CS,FX803
        lda #WRITE_GEN1
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

; DESACTIVA EL TRANSMISOR

        BCLR CS,FX803    ;CHIP SELECT
        LDA #WRITE_GEN1
        JSR SPIOUT
        LDA #$20
        JSR SPIOUT
        LDA #$00
        JSR SPIOUT
        BSET CS,FX803    ;CHIP SELECT

        BCLR CS,FX803    ;CHIP SELECT
        LDA #WRITE_GEN2
        JSR SPIOUT
        LDA #20
        JSR SPIOUT
        LDA #00
        JSR SPIOUT
        BSET CS,FX803    ;CHIP SELECT

        bclr CS,FX803
        lda #WRITE_CONTROL_REG
        jsr SpiOut
        bclr GP_TIMER_INT,CtrlReg
        bset AUDIO_SWITCH,CtrlReg
        bclr SUMMING_SWITCH,CtrlReg
        bclr DECODER_INT,CtrlReg    ;No permite interrupts del decodificador

        lda CtrlReg
        jsr SpiOut
        bset CS,FX803

        bclr BT_PTT,PORTB       ;Quita el PTT

wt2     jsr RqBih
        bcs wt2

        jsr Ini803
        jmp main

RqBih:
        lda #$10
rqb1    bih rqb2
        deca
        bne rqb1
        sec
        rts

rqb2    lda #$10
rqb3   bil rqbih
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

;------------------------------ CALCULA LA RESTA 511-Rh ------------------------
FORMULA:

;EL NUMERO A RESTAR HA DE VENIR EN RhL

        LDA #$FF
        SUB RhL
        STA RhL
        LDA #$1
        STA RhH

;RESULTADO =
;       RhH BYTE ALTO (SIEMPRE 1)
;       Rhl BYTE BAJO

;-------------------------------------------------------------------------------

;-------------------------------- MULTIPLICA 28000 x Nh ------------------------

        LDA #$6D
        STA MLTNDO0
        LDA #$60
        STA MLTNDO1             ;6D60H = 28000d
        LDA Nh
        STA MLTDOR
        JSR MULTI               ;MULTIPLICO 28000 x Nh

;-------------------------------------------------------------------------------

;------------------- DIVIDE (28000xNh) / (511-Rh) ------------------------------

        LDA RESULT2             ;EL RESULTADO QUEDA EN RESULT0, RESULT1, RESULT2
        STA R3
        LDA RESULT1
        STA R0
        LDA RESULT0
        STA R1

        LDA #$00
        STA DVSOR0
        LDA RhH
        STA DVSOR1
        LDA RhL
        STA DVSOR2
        JSR DIV

        LDA R3
        AND #$F0
        RORA
        RORA
        RORA
        RORA
        STA TEMP

        LDA R0
        ROLA
        ROLA
        ROLA
        ROLA
        ORA TEMP
        STA R0
        LDA R3
        AND #$0F        ;MULTIPLICA POR 10 EL RESULTADO DE LA FORMULA PARA DAR
        STA R3          ;MAS PRECISION.
        RTS

; ---------------------------------------- DIV ---------------------------------
;SUBRUTINA PARA DIVIDIR.
;EN DVSOR1 (BYTE ALTO) Y DIVSOR2 (BAJO) VA EL DIVISOR
;EN R1 (BYTE ALTO) Y R0 (BAJO) EL DIVIDENDO
;EL RESULTADO QUEDA EN R1 Y R0

DIV:
        COM DVSOR0
        COM DVSOR1
        COM DVSOR2      ;COMPLEMENTA DIVISOR
        CLC
        LDA #$1         ;INCREM.DIVISOR PARA DOS COMPLEM.
        ADC DVSOR2
        STA DVSOR2
        CLRA
        ADC DVSOR1
        STA DVSOR1
        CLRA
        ADC DVSOR0
        STA DVSOR0
        CLR TEMP0
        CLR TEMP1       ;VALOR INICIAL PARA RESTO
        CLR TEMP2
        LDX #25         ;CONTADOR
DV0:
        CLC             ;SUMA DVSOR1-2 A TEMP1-2
        LDA TEMP2
        ADC DVSOR2
        STA TEMP4
;
        LDA TEMP1
        ADC DVSOR1
        STA TEMP3
        LDA TEMP0
        ADC DVSOR0
        STA TEMP5
;
        BCC DV1
        LDA TEMP4
        STA TEMP6
        LDA TEMP2
        STA TEMP4
        LDA TEMP6
        STA TEMP2
        LDA TEMP3
        STA TEMP6
        LDA TEMP1
        STA TEMP3
        LDA TEMP6
        STA TEMP1
        LDA TEMP5
        STA TEMP6
        LDA TEMP0
        STA TEMP5
        LDA TEMP6
        STA TEMP0

DV1:
        ROL R3
        ROL R0          ;ROTA BYTE BAJO DIVIDENDO
        ROL R1
        ROL TEMP2
        ROL TEMP1
        ROL TEMP0
        DECX
        BNE DV0
;                       TRAS LA DIVISION ENTERA, ROTA RESTO Y LO DEJA EN TEMP1-2
        ROR TEMP0
        ROR TEMP1
        ROR TEMP2
        RTS
;
;-------------------------------------------------------------------------------

;------------------------------------------ MULTI-------------------------------
; EN MLTNDO0 BYTE ALTO MULTIPLICADOR
; EN MLTNDO1 BYTE BAJO MULTIPLICADOR
; EN MLTDOR MULTIPLICANDO.
; RESULT0, RESULT1, RESULT2 RESULTADO

multi:
        CLR RESULT0
        CLR RESULT1
        CLR RESULT2

        LDA MLTNDO1
        LDX MLTDOR

        MUL

        STX RESULT1
        STA RESULT2     ;RESULTADO BYTE BAJO FINAL

        STX SAVA

        LDA MLTNDO0
        LDX MLTDOR
        MUL
        STA RESULT1
        CLC
        LDA SAVA
        ADD RESULT1
        BCS HayCarry

        STA RESULT1
        STX RESULT0
        RTS

HayCarry:

        INC RESULT0
        LDX MLTDOR
        MUL
        STA RESULT1

        TXA
        ADD RESULT0
        LDX MLTDOR
        MUL

        STA RESULT0
        RTS
;-------------------------------------------------------------------------------

;------------------------------------ SUBRUTINAS GESTION 803 -------------------

;- - - - - - - - - - - - - - - - - - - - INICIALIZA EL 803 - - - - - - - - - - -
INI803:

;PRIMERO HACE UN RESET DEL 803
;

        BCLR CS,FX803
        LDA #GENERAL_RESET
        JSR SPIOUT
        BSET CS,FX803

;TRANSMITE UN 00 AL CONTROL REGISTER
;
;  BITS                 SIGNIFICADO
;
;   7                AUDI SWITCH          0 = DISABLE
;   6                G/PURPOSE TIMER      0 = DISABLE
;   5                DECODER INTERRUPTS   0 = DISABLE
;   4                SUMMING SWITCH       1 = ENABLE
;   3-2              BAND SELECTION     0 0 = HIGH BAND
;   1-0              NO SE USA          0 0
;
        BCLR CS,FX803    ;CHIP SELECT
        LDA #WRITE_CONTROL_REG      ;DIRECCION CONTROL REGISTER
        JSR SPIOUT
        lda #$80                    ;INSTRUCCION CONTROL REGISTER
        sta CtrlReg
        JSR SPIOUT
        BSET CS,FX803

; DESACTIVA EL TRANSMISOR

        BCLR CS,FX803    ;CHIP SELECT
        LDA #WRITE_GEN1
        JSR SPIOUT
        LDA #$20
        JSR SPIOUT
        LDA #$00
        JSR SPIOUT
        BSET CS,FX803    ;CHIP SELECT

        BCLR CS,FX803    ;CHIP SELECT
        LDA #WRITE_GEN2
        JSR SPIOUT
        LDA #20
        JSR SPIOUT
        LDA #00
        JSR SPIOUT
        BSET CS,FX803    ;CHIP SELECT

;PROGRAMA EL NOTONE REGISTER CON 20 ms

        BCLR CS,FX803    ;CHIP SELECT
        LDA #WRITE_NOTONE_TIMER     ;DIRECCION NO TONE REGISTER
        JSR SPIOUT
        LDA #$02
        JSR SPIOUT      ;TIEMPO MAXIMO
        BSET CS,FX803

        BCLR CS,FX803    ;CHIP SELECT
        LDA #WRITE_CONTROL_REG
        JSR SPIOUT
        bclr DECODER_INT,CtrlReg    ;No permite interrupts del decodificador
        lda CtrlReg
        JSR SPIOUT
        BSET CS,FX803
        RTS

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;- - - - - - - - TRANSMITE AL SPI EL CONTENIDO DE A - - - - - - - - - - - - -

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

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RdStatus:

        bclr CS,FX803
        lda #$31
        jsr SpiOut
        jsr SpiIn
        sta temp
        bset CS,FX803
        rts

;- - - - - - - - - LEE UN BYTE DEL SPI Y LO PONE EN A - - - - - - - - - - - -
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
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;//-------------- Convierte un n£mero binario a ascii decimal -----------------
;// Convierte un n£mero binario a una cadena de car cteres ascii
;// El n£mero ha de estar en result
;// La cadena la pone en string

hex2asc:
        clr sava
        clr savx
        clr PntResta
BucHex
        jsr resta
        bcs HayC

        lda ResultRes
        sta Result
        lda ResultRes+1
        sta Result+1
        lda ResultRes+2
        sta Result+2
        lda ResultRes+3
        sta Result+3
        lda ResultRes+4
        sta Result+4
        lda ResultRes+5
        sta Result+5
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
;----------------------------------- RESTA -----------------------------------
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
        sta ResultRes,x
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

;-----------------------------------------------------------------------------

IRQ:
     rti

;=============================================================================

;=========================  TABLAS Y MENSAJES ================================

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

EEA_TX:

        fcb 001h,0fch;  '0'
        fcb 003h,080h;  '1'
        fcb 003h,04ah;  '2'
        fcb 003h,016h;  '3'
        fcb 002h,0e6h;  '4'
        fcb 002h,0b9h;  '5'
        fcb 002h,08eh;  '6'
        fcb 002h,066h;  '7'
        fcb 002h,040h;  '8'
        fcb 002h,01dh;  '9'
        fcb 003h,0bbh;  'G'
        fcb 001h,0ddh;  'R'

EEA_RX:

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

ZVI_RX:

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

ZVI_TX:
        fcb 01h,0a4h    ;0
        fcb 03h,0b6h    ;1
        fcb 03h,064h    ;2
        fcb 03h,019h    ;3
        fcb 02h,0d0h    ;4
        fcb 02h,092h    ;5
        fcb 02h,05bh    ;6
        fcb 02h,026h    ;7
        fcb 01h,0f8h    ;8
        fcb 01h,0cah    ;9
        fcb 00h,00h;    ;G
        fcb 01h,083h    ;R

timer:
swi:
        rti
;===============================================================================

   org $7f1
   db $06           ;MOR set for PORT A ints and LEVEL ints

   org $7f8
   dw timer
   dw irq
   dw swi
   dw start

   end
