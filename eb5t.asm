;*******************************************************************************
; NOMBRE : CENDEC5T.SRC
; DESCRIP: Codificador/decodificador tonos ZVI
; MICRO. : MC68HC705J1A
; OBSERV.: - Controla el FX803 por el SPI.
; - Con filtro.
;*******************************************************************************

RAM                 equ       $80

PORTA               equ       0
DDRA                equ       1
PORTB               equ       2
DDRB                equ       3

;*******************************************************************************

GENERAL_RESET       equ       $01
WRITE_CONTROL_REG   equ       $30
READ_STATUS_REG     equ       $31
READ_RX_TONE        equ       $32
WRITE_NOTONE_TIMER  equ       $33
WRITE_GEN1          equ       $34
WRITE_GEN2          equ       $35
WRITE_GP_TIMER      equ       $36

AUDIO_SWITCH        equ       7
GP_TIMER_INT        equ       6
DECODER_INT         equ       5
SUMMING_SWITCH      equ       4
HIGH_BAND           equ       0
MID_BAND            equ       4
EXTENDED_BAND       equ       8

CS                  equ       2
SCK                 equ       5
RDATA               equ       4
CD                  equ       3

FX803               equ       1

BT_PTT              equ       1

;*******************************************************************************
                    #RAM
;*******************************************************************************

flag                rmb       1

sava                rmb       1
savx                rmb       1
temp0               rmb       1
temp                rmb       1
nh                  rmb       1
rhh                 rmb       1
rhl                 rmb       1
result0             rmb       1
result1             rmb       1
result2             rmb       1
result3             rmb       1
mltndo0             rmb       1
mltndo1             rmb       1
mltdor              rmb       1
r0                  rmb       1
r1                  rmb       1
r3                  rmb       1
temp1               rmb       1
temp2               rmb       1
dvsor1              rmb       1
dvsor2              rmb       1
temp3               rmb       1
temp4               rmb       1
temp5               rmb       1
dvsor0              rmb       1
temp6               rmb       1
ctrl_reg            rmb       1

cnt_byte            rmb       1
carr                rmb       1
result              rmb       6
pnt_resta           rmb       1
result_res          rmb       6
string              rmb       10
ant_char            rmb       1

;*******************************************************************************
                    #ROM
;*******************************************************************************

                    bra       Start

indic               fcb       2,5
tiempo              fcb       2

Start
                    clrx

                    clr       DDRA

                    lda       #$2e
                    sta       DDRB

                    clr       PORTA
                    clr       PORTB

                    clrx
bc
                    clra
                    sta       RAM,x               ; Inicializa la memoria con 00
                    incx
                    cpx       #$40
                    bne       bc

                    bset      CS,FX803
                    sei
                    jsr       Ini803

main
                    bil       ptt

                    jsr       RdStatus            ; MIRA EL ESTADO DEL 803, EL STATUS ESTA EN temp
                    brset     0,temp,TonVal
                    brset     1,temp,not
; brset 2,temp,GpTimer

                    bra       main

TonVal                                            ; HA RECIBIDO UN TONO VALIDO.

                    bclr      CS,FX803
                    lda       #READ_RX_TONE
                    jsr       SpiOut
                    jsr       SpiIn
                    sta       nh
                    jsr       SpiIn
                    sta       rhl
                    bset      CS,FX803
                    jsr       Formula
bs
                    clrx
bsc
                    lda       EEA_RX,x
                    beq       NoExis              ; ESTE TONO NO EXISTE EN LA TABLA.

                    cmp       r1
                    beq       EsNum
                    lda       r1
                    sub       #$2
                    cmp       EEA_RX,x
                    bcc       sbsc
                    add       #$4
                    cmp       EEA_RX,x
                    bpl       EsNum
sbsc                incx
                    bra       bsc

NoExis
                    bra       main

EsNum
                    lda       EEA_RX,x
                    bra       main

not

; PROGRAMA EL NOTONE REGISTER CON 40 ms

                    bclr      CS,FX803            ; CHIP SELECT
                    lda       #WRITE_NOTONE_TIMER  ; DIRECCION NO TONE REGISTER
                    jsr       SPIOUT
                    lda       tiempo
                    jsr       SPIOUT              ; TIEMPO MAXIMO
                    bset      CS,FX803
                    bra       main

ptt
                    jsr       RqBih
                    bcc       main

                    bset      BT_PTT,PORTB        ; Pone PTT
                    jsr       bucle
                    jsr       bucle
                    jsr       bucle
                    jsr       bucle
                    jsr       bucle

                    bclr      CS,FX803
                    lda       #WRITE_CONTROL_REG
                    jsr       SpiOut
                    bclr      GP_TIMER_INT,ctrl_reg
                    bclr      AUDIO_SWITCH,ctrl_reg
                    bset      SUMMING_SWITCH,ctrl_reg
                    bclr      DECODER_INT,ctrl_reg  ; No permite interrupts del decodificador
                    lda       ctrl_reg
                    jsr       SpiOut
                    bset      CS,FX803

                    bclr      CS,FX803            ; CHIP SELECT
                    lda       #WRITE_NOTONE_TIMER  ; DIRECCION NO TONE REGISTER
                    jsr       SPIOUT
                    clra
                    jsr       SPIOUT
                    bset      CS,FX803

                    bclr      CS,FX803
                    lda       #WRITE_GP_TIMER
                    jsr       SpiOut
                                                  ; 02 para 20 ms
                                                  ; 04 para 40 ms
                                                  ; 07 para 70 ms
                    lda       tiempo              ; Programa ms para timer
                    jsr       SpiOut
                    bset      CS,FX803
Wt0
                    jsr       RdStatus
                    brclr     2,temp,Wt0

                    lda       PORTB
                    and       #$01
                    sta       result+4
                    lda       PORTA
                    sta       result+5

                    clc
                    lda       result+5
                    inca
                    sta       result+5
                    lda       result+4
                    adc       #$0
                    sta       result+4

                    jsr       HexToAsc

                    clr       ant_char
                    ldx       #5

; cabecera para SERVI TAXI '25'

                    lda       indic
                    add       #$30
                    sta       string,x
                    incx
                    lda       indic+1
                    add       #$30
                    sta       string,x

                    ldx       #$5

snd
                    lda       string,x
                    cmp       ant_char
                    bne       NoIgual

                    lda       #$3b
                    sta       ant_char
                    bra       SiIgual

NoIgual             sta       ant_char
SiIgual             sub       #$30
                    stx       savx
                    sta       sava

                    lda       sava
                    ldx       #2
                    mul
                    tax

                    lda       ZVI_TX,x
                    sta       temp
                    incx
                    lda       ZVI_TX,x
                    sta       temp1

                    bclr      CS,FX803
                    lda       #WRITE_GEN1
                    jsr       SpiOut
                    lda       temp
                    jsr       SpiOut
                    lda       temp1
                    jsr       SpiOut
                    bset      CS,FX803

wt1
                    jsr       RdStatus
                    brclr     2,temp,wt1

                    ldx       savx
                    incx
                    cpx       #$a
                    bne       snd

; DESACTIVA EL TRANSMISOR

                    bclr      CS,FX803            ; CHIP SELECT
                    lda       #WRITE_GEN1
                    jsr       SPIOUT
                    lda       #$20
                    jsr       SPIOUT
                    lda       #$00
                    jsr       SPIOUT
                    bset      CS,FX803            ; CHIP SELECT

                    bclr      CS,FX803            ; CHIP SELECT
                    lda       #WRITE_GEN2
                    jsr       SPIOUT
                    lda       #20
                    jsr       SPIOUT
                    lda       #00
                    jsr       SPIOUT
                    bset      CS,FX803            ; CHIP SELECT

                    bclr      CS,FX803
                    lda       #WRITE_CONTROL_REG
                    jsr       SpiOut
                    bclr      GP_TIMER_INT,ctrl_reg
                    bset      AUDIO_SWITCH,ctrl_reg
                    bclr      SUMMING_SWITCH,ctrl_reg
                    bclr      DECODER_INT,ctrl_reg  ; No permite interrupts del decodificador

                    lda       ctrl_reg
                    jsr       SpiOut
                    bset      CS,FX803

                    bclr      BT_PTT,PORTB        ; Quita el PTT

wt2                 bsr       RqBih
                    bcs       wt2

                    jsr       Ini803
                    jmp       main

RqBih
                    lda       #$10
rqb1                bih       rqb2
                    deca
                    bne       rqb1
                    sec
                    rts

rqb2                lda       #$10
rqb3                bil       RqBih
                    deca
                    bne       rqb3
                    clc
                    rts

bucle
                    ldx       #$3f
bu0                 lda       #$ff
bu1                 nop:3
                    deca
                    bne       bu1
                    decx
                    bne       bu0
                    rts

; ------------------------------ CALCULA LA RESTA 511-Rh ------------------------
Formula

; EL NUMERO A RESTAR HA DE VENIR EN rhl

                    lda       #$FF
                    sub       rhl
                    sta       rhl
                    lda       #$1
                    sta       rhh

; RESULTADO =
; rhh BYTE ALTO (SIEMPRE 1)
; rhl BYTE BAJO

; -------------------------------------------------------------------------------

; -------------------------------- MULTIPLICA 28000 x nh ------------------------

                    lda       #$6D
                    sta       mltndo0
                    lda       #$60
                    sta       mltndo1             ; 6D60H = 28000d
                    lda       nh
                    sta       mltdor
                    jsr       Multi               ; MULTIPLICO 28000 x nh

; -------------------------------------------------------------------------------

; ------------------- DIVIDE (28000xNh) / (511-Rh) ------------------------------

                    lda       result2             ; EL RESULTADO QUEDA EN result0, result1, result2
                    sta       r3
                    lda       result1
                    sta       r0
                    lda       result0
                    sta       r1

                    lda       #$00
                    sta       dvsor0
                    lda       rhh
                    sta       dvsor1
                    lda       rhl
                    sta       dvsor2
                    bsr       DIV

                    lda       r3
                    and       #$F0
                    rora
                    rora
                    rora
                    rora
                    sta       temp

                    lda       r0
                    rola
                    rola
                    rola
                    rola
                    ora       temp
                    sta       r0
                    lda       r3
                    and       #$0F                ; MULTIPLICA POR 10 EL RESULTADO DE LA FORMULA PARA DAR
                    sta       r3                  ; MAS PRECISION.
                    rts

; ---------------------------------------- DIV ---------------------------------
; SUBRUTINA PARA DIVIDIR.
; EN dvsor1 (BYTE ALTO) Y DIVSOR2 (BAJO) VA EL DIVISOR
; EN r1 (BYTE ALTO) Y r0 (BAJO) EL DIVIDENDO
; EL RESULTADO QUEDA EN r1 Y r0

DIV
                    com       dvsor0
                    com       dvsor1
                    com       dvsor2              ; COMPLEMENTA DIVISOR
                    clc
                    lda       #$1                 ; INCREM.DIVISOR PARA DOS COMPLEM.
                    adc       dvsor2
                    sta       dvsor2
                    clra
                    adc       dvsor1
                    sta       dvsor1
                    clra
                    adc       dvsor0
                    sta       dvsor0
                    clr       temp0
                    clr       temp1               ; VALOR INICIAL PARA RESTO
                    clr       temp2
                    ldx       #25                 ; CONTADOR
DV0
                    clc                           ; SUMA dvsor1-2 A temp1-2
                    lda       temp2
                    adc       dvsor2
                    sta       temp4

                    lda       temp1
                    adc       dvsor1
                    sta       temp3
                    lda       temp0
                    adc       dvsor0
                    sta       temp5

                    bcc       DV1
                    lda       temp4
                    sta       temp6
                    lda       temp2
                    sta       temp4
                    lda       temp6
                    sta       temp2
                    lda       temp3
                    sta       temp6
                    lda       temp1
                    sta       temp3
                    lda       temp6
                    sta       temp1
                    lda       temp5
                    sta       temp6
                    lda       temp0
                    sta       temp5
                    lda       temp6
                    sta       temp0

DV1
                    rol       r3
                    rol       r0                  ; ROTA BYTE BAJO DIVIDENDO
                    rol       r1
                    rol       temp2
                    rol       temp1
                    rol       temp0
                    decx
                    bne       DV0
; TRAS LA DIVISION ENTERA, ROTA RESTO Y LO DEJA EN temp1-2
                    ror       temp0
                    ror       temp1
                    ror       temp2
                    rts

; -------------------------------------------------------------------------------

; ------------------------------------------ Multi-------------------------------
; EN mltndo0 BYTE ALTO MULTIPLICADOR
; EN mltndo1 BYTE BAJO MULTIPLICADOR
; EN mltdor MULTIPLICANDO.
; result0, result1, result2 RESULTADO

Multi
                    clr       result0
                    clr       result1
                    clr       result2

                    lda       mltndo1
                    ldx       mltdor

                    mul

                    stx       result1
                    sta       result2             ; RESULTADO BYTE BAJO FINAL

                    stx       sava

                    lda       mltndo0
                    ldx       mltdor
                    mul
                    sta       result1
                    clc
                    lda       sava
                    add       result1
                    bcs       HayCarry

                    sta       result1
                    stx       result0
                    rts

HayCarry

                    inc       result0
                    ldx       mltdor
                    mul
                    sta       result1

                    txa
                    add       result0
                    ldx       mltdor
                    mul

                    sta       result0
                    rts

; -------------------------------------------------------------------------------

; ------------------------------------ SUBRUTINAS GESTION 803 -------------------

; - - - - - - - - - - - - - - - - - - - - INICIALIZA EL 803 - - - - - - - - - - -
Ini803

; PRIMERO HACE UN RESET DEL 803

                    bclr      CS,FX803
                    lda       #GENERAL_RESET
                    jsr       SPIOUT
                    bset      CS,FX803

; TRANSMITE UN 00 AL CONTROL REGISTER

; BITS                 SIGNIFICADO

; 7                AUDI SWITCH          0 = DISABLE
; 6                G/PURPOSE TIMER      0 = DISABLE
; 5                DECODER INTERRUPTS   0 = DISABLE
; 4                SUMMING SWITCH       1 = ENABLE
; 3-2              BAND SELECTION     0 0 = HIGH BAND
; 1-0              NO SE USA          0 0

                    bclr      CS,FX803            ; CHIP SELECT
                    lda       #WRITE_CONTROL_REG  ; DIRECCION CONTROL REGISTER
                    jsr       SPIOUT
                    lda       #$80                ; INSTRUCCION CONTROL REGISTER
                    sta       ctrl_reg
                    jsr       SPIOUT
                    bset      CS,FX803

; DESACTIVA EL TRANSMISOR

                    bclr      CS,FX803            ; CHIP SELECT
                    lda       #WRITE_GEN1
                    jsr       SPIOUT
                    lda       #$20
                    jsr       SPIOUT
                    lda       #$00
                    jsr       SPIOUT
                    bset      CS,FX803            ; CHIP SELECT

                    bclr      CS,FX803            ; CHIP SELECT
                    lda       #WRITE_GEN2
                    jsr       SPIOUT
                    lda       #20
                    jsr       SPIOUT
                    lda       #00
                    jsr       SPIOUT
                    bset      CS,FX803            ; CHIP SELECT

; PROGRAMA EL NOTONE REGISTER CON 20 ms

                    bclr      CS,FX803            ; CHIP SELECT
                    lda       #WRITE_NOTONE_TIMER  ; DIRECCION NO TONE REGISTER
                    jsr       SPIOUT
                    lda       #$02
                    jsr       SPIOUT              ; TIEMPO MAXIMO
                    bset      CS,FX803

                    bclr      CS,FX803            ; CHIP SELECT
                    lda       #WRITE_CONTROL_REG
                    jsr       SPIOUT
                    bclr      DECODER_INT,ctrl_reg  ; No permite interrupts del decodificador
                    lda       ctrl_reg
                    jsr       SPIOUT
                    bset      CS,FX803
                    rts

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; - - - - - - - - TRANSMITE AL SPI EL CONTENIDO DE A - - - - - - - - - - - - -

SpiOut

                    ldx       #$8
                    clc

loop                rola
                    bcs       uno

                    bclr      CD,FX803
clk                 bclr      SCK,FX803
                    bset      SCK,FX803
                    decx
                    cpx       #$00
                    bne       loop
                    rts

uno                 bset      CD,FX803
                    bra       clk

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RdStatus

                    bclr      CS,FX803
                    lda       #$31
                    bsr       SpiOut
                    bsr       SpiIn
                    sta       temp
                    bset      CS,FX803
                    rts

; - - - - - - - - - LEE UN BYTE DEL SPI Y LO PONE EN A - - - - - - - - - - - -
SpiIn

                    ldx       #$8
                    clc
                    clra

loo2                bclr      SCK,FX803
                    bset      SCK,FX803
                    brset     RDATA,FX803,EsUno
                    clc

clk2                rola
                    decx
                    cpx       #$00
                    bne       loo2
                    rts

EsUno               sec
                    bra       clk2

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; //-------------- Convierte un n£mero binario a ascii decimal -----------------
; // Convierte un n£mero binario a una cadena de car cteres ascii
; // El n£mero ha de estar en result
; // La cadena la pone en string

HexToAsc
                    clr       sava
                    clr       savx
                    clr       pnt_resta
BucHex
                    bsr       resta
                    bcs       HayC

                    lda       result_res
                    sta       result
                    lda       result_res+1
                    sta       result+1
                    lda       result_res+2
                    sta       result+2
                    lda       result_res+3
                    sta       result+3
                    lda       result_res+4
                    sta       result+4
                    lda       result_res+5
                    sta       result+5
                    inc       sava

BucH
                    lda       pnt_resta
                    cmp       #$32
                    bne       BucHex
                    rts

HayC

                    ldx       savx
                    lda       sava
                    add       #'0'
                    sta       string,x
                    inc       savx
                    clr       sava
                    lda       pnt_resta
                    add       #$5
                    sta       pnt_resta
                    bra       BucH

; ----------------------------------- RESTA -----------------------------------
resta

                    clr       carr
                    lda       #$5
                    sta       cnt_byte
                    lda       pnt_resta
                    add       #$4
                    sta       pnt_resta
                    tax

restB
                    ldx       pnt_resta
                    dec       pnt_resta
                    lda       const,x
                    add       carr
                    sta       temp
                    clr       carr

                    ldx       cnt_byte
                    lda       result,x
                    sub       temp
                    bcc       NoCarry

                    inc       carr

NoCarry
                    ldx       cnt_byte
                    sta       result_res,x
                    dec       cnt_byte
                    lda       cnt_byte
                    bne       restB
                    inc       pnt_resta
                    clc
                    lda       carr
                    beq       FinRes
                    sec
FinRes
                    rts

; -----------------------------------------------------------------------------

IRQ_Handler
                    rti

; =============================================================================

; =========================  TABLAS Y MENSAJES ================================

const

                    fcb       0,$3b,$9a,$0ca,0
                    fcb       0,$5,$0f5,$0e1,0
                    fcb       0,0,$98,$96,$80
                    fcb       0,0,$0f,$42,$40
                    fcb       0,0,1,$86,$a0
                    fcb       0,0,$00,$27,$10
                    fcb       0,0,$00,$3,$e8
                    fcb       0,0,$00,$00,$64
                    fcb       0,0,$00,$00,$0a
                    fcb       0,0,$00,$00,$01

EEA_TX
                    fcb       $01,$fc;            ; '0'
                    fcb       $03,$80;            ; '1'
                    fcb       $03,$4a;            ; '2'
                    fcb       $03,$16;            ; '3'
                    fcb       $02,$e6;            ; '4'
                    fcb       $02,$b9;            ; '5'
                    fcb       $02,$8e;            ; '6'
                    fcb       $02,$66;            ; '7'
                    fcb       $02,$40;            ; '8'
                    fcb       $02,$1d;            ; '9'
                    fcb       $03,$bb;            ; 'G'
                    fcb       $01,$dd;            ; 'R'

EEA_RX
                    fcb       $7b
                    fcb       $46
                    fcb       $4a
                    fcb       $4f
                    fcb       $54
                    fcb       $5a
                    fcb       $60
                    fcb       $66
                    fcb       $6c
                    fcb       $74
                    fcb       $83
                    fcb       00

ZVI_RX
                    fcb       $95
                    fcb       $42
                    fcb       $48
                    fcb       $4f
                    fcb       $57
                    fcb       $5f
                    fcb       $68
                    fcb       $72
                    fcb       $7c
                    fcb       $89
                    fcb       $a2
                    fcb       00

ZVI_TX
                    fcb       $01,$a4             ; 0
                    fcb       $03,$b6             ; 1
                    fcb       $03,$64             ; 2
                    fcb       $03,$19             ; 3
                    fcb       $02,$d0             ; 4
                    fcb       $02,$92             ; 5
                    fcb       $02,$5b             ; 6
                    fcb       $02,$26             ; 7
                    fcb       $01,$f8             ; 8
                    fcb       $01,$ca             ; 9
                    fcb       $00,$00             ; G
                    fcb       $01,$83             ; R

Timer_Handler
SWI_Handler         rti

;*******************************************************************************
                    org       $7f1
                    db        $06                 ; MOR set for PORT A ints and LEVEL ints

                    org       $7f8
                    dw        Timer_Handler
                    dw        IRQ_Handler
                    dw        SWI_Handler
                    dw        Start

                    end

;*******************************************************************************

SPIOUT              def       :AnRTS
