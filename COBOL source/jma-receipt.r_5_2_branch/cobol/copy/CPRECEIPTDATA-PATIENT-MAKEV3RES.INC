      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      ******************************************************************
      *    ���Уɥ쥻�ťǡ�������ʸ��̥쥻�ťǡ���������(xml2)
      *                        COPY (CPRECEIPTDATA-PATIENT-MAKEV3RES.INC)
      ******************************************************************
       01  XML-RECEIPTDATA-PATIENT-MAKERES.
           02  RECEDMAKES-CONTEXT          PIC S9(9)   BINARY.
           02  RECEDMAKES-OBJECT           PIC X(36).
           02  RECEDMAKES-MODE             PIC S9(9)   BINARY.
           02  RECEDMAKES-RECORDNAME       PIC X(128).
           02  RECEDMAKES-RECEIPT-MAKE-RES.
      *        �ꥯ�������ֹ�
               03  RECEDMAKES-REQUEST-NUMBER   PIC X(2).
      *
               03  RECEDMAKES-RESPONS-NUMBER   PIC X(2).
      *        �����UID
               03  RECEDMAKES-KARTE-UID    PIC X(36).
      *        ���륫UID
               03  RECEDMAKES-ORCA-UID     PIC X(36).
      *        �»���
               03  RECEDMAKES-INFORMATION-DATE
                                           PIC X(10).
      *        �»ܻ���
               03  RECEDMAKES-INFORMATION-TIME
                                           PIC X(8).
      *        �ֵѥ�����
               03  RECEDMAKES-API-RESULT   PIC X(3).
      *        �ֵѥ�å�����
               03  RECEDMAKES-API-RESULT-MSG
                                           PIC X(100).
      *
               03  RECEDMAKES-RESKEY       PIC X(100).
      *
               03  RECEDMAKES-SUBMISSION-MODE
                                           PIC X(2).
               03  RECEDMAKES-PERFORM-MONTH
                                           PIC X(7).
               03  RECEDMAKES-START-MONTH  PIC X(7).
               03  RECEDMAKES-END-MONTH    PIC X(7).
               03  RECEDMAKES-HKNJANUM     PIC X(8).
      *
               03  RECEDMAKES-DATA-ID-INFORMATION  OCCURS  10.
                   05  RECEDMAKES-DATA-ID  PIC X(37).
                   05  RECEDMAKES-PRINT-ID PIC X(37).
      *
      *        ��˥�
               03  RECEDMAKES-API-WARNING-INFO     OCCURS  5.
                   05  RECEDMAKES-API-WARNING
                                           PIC X(3).
                   05  RECEDMAKES-API-WARNING-MSG
                                           PIC X(100).
      *        ���顼���԰���
               03  RECEDMAKES-ERR-OVERFLOW PIC X(04).
               03  RECEDMAKES-ERR-INFORMATION      OCCURS  100.
                   05  RECEDMAKES-ITEM-POSITION
                                           PIC X(3).
                   05  RECEDMAKES-PATIENT-ID
                                           PIC X(20).
                   05  RECEDMAKES-PATIENT-MONTH
                                           PIC X(7).
                   05  RECEDMAKES-PATIENT-INOUT
                                           PIC X(2).
                   05  RECEDMAKES-ERROR-CODE
                                           PIC X(3).
                   05  RECEDMAKES-ERROR-MSG
                                           PIC X(100).
                   05  RECEDMAKES-DETAIL-INFORMATION   OCCURS  5.
                     06  RECEDMAKES-DETAIL-ITEM-POSITION
                                           PIC X(2).
                     06  RECEDMAKES-DETAIL-INFORMATION-G.
      *                ���̥�����
                       07  RECEDMAKES-DETAIL-CODE
                                           PIC X(04).
      *                ����
                       07  RECEDMAKES-DETAIL-TYPE
                                           PIC X(12).
                       07  RECEDMAKES-DETAIL-INOUT
                                           PIC X(1).
      *                �ݸ�����
                       07  RECEDMAKES-MAIN-INSURANCE.
      *                    �ݸ��ɣ�
                           09  RECEDMAKES-HKNID
                                           PIC X(10).
      *                    �ݸ��ֹ�
                           09  RECEDMAKES-INSURANCE-CLASS
                                           PIC X(03).
      *                    �ݸ����ֹ�
                           09  RECEDMAKES-INSURANCE-NUMBER
                                           PIC X(08).
      *                    ����
                           09  RECEDMAKES-MAIN-MARK
                                           PIC X(38).
      *                    �ֹ�
                           09  RECEDMAKES-MAIN-NUMBER
                                           PIC X(38).
      *                    �����ʬ
                           09  RECEDMAKES-MAIN-ASSISTANCE
                                           PIC X(01).
      *                �������
                       07  RECEDMAKES-PUBLIC-INSURANCE   OCCURS  4.
      *                    ����ɣ�
                           09  RECEDMAKES-KOHID
                                           PIC X(10).
      *                    �����ֹ�
                           09  RECEDMAKES-KOHNUM
                                           PIC X(03).
      *                    ��ô���ֹ�
                           09  RECEDMAKES-PROVIDER-NUMBER
                                           PIC X(08).
      *                    ������ֹ�
                           09  RECEDMAKES-RECIPIENT-NUMBER
                                           PIC X(20).
      *                �ҳ�������̵ͭ
                       07  RECEDMAKES-ACCIDENT-TARGET
                                           PIC X(4).
      *                �������̵ͭ
                       07  RECEDMAKES-INCARE-CLASS
                                           PIC X(4).
      *                ϫ�Ҿ���
                       07  RECEDMAKES-ACCIDENT-INSURANCE.
      *****************���¤�����
      *****************    09  RECEDMAKES-DISEASE-LOCATION
      *****************                    PIC X(100).
      *                    ϫ�Ҽ����ݸ���ʬ
                           09  RECEDMAKES-ACCIDENT
                                           PIC X(1).
      *                    ϫƯ�ݸ��ֹ�
                           09  RECEDMAKES-ACCIDENT-NUMBER
                                           PIC X(14).
      *                     ǯ��ڽ��ֹ�
                           09  RECEDMAKES-PENSIONCERTIFICATE
                                           PIC X(09).
      *                    �ҳ���ʬ
                           09  RECEDMAKES-ACCIDENT-CLASS
                                           PIC X(1).
      *                    �򹯴�����Ģ�ֹ�
                           09  RECEDMAKES-KANRITCHO-NUMBER
                                          PIC X(13).
      *                    ����ǯ����
                           09  RECEDMAKES-RYOYO-DD
                                           PIC X(2).
      *
                       07  RECEDMAKES-DETAIL-ERROR-CODE
                                           PIC X(3).
                       07  RECEDMAKES-DETAIL-ERROR-MSG
                                           PIC X(100).
      *        �������
               03  RECEDMAKES-SELECT-OVERFLOW
                                           PIC X(04).
               03  RECEDMAKES-SELECT-INFORMATION       OCCURS  100.
                   05  RECEDMAKES-S-ITEM-POSITION
                                           PIC X(3).
                   05  RECEDMAKES-S-PATIENT-ID
                                           PIC X(20).
                   05  RECEDMAKES-S-PATIENT-MONTH
                                           PIC X(7).
                   05  RECEDMAKES-S-PATIENT-INOUT
                                           PIC X(2).
                   05  RECEDMAKES-S-DETAIL-INFORMATION OCCURS  5.
                     06  RECEDMAKES-S-DETAIL-POSITION
                                           PIC X(2).
                     06  RECEDMAKES-S-RE-DETAIL-POSITION
                                                       OCCURS  20.
      *                ���̥�����
                       07  RECEDMAKES-S-DETAIL-CODE
                                           PIC X(04).
      *                ����
                       07  RECEDMAKES-S-DETAIL-TYPE
                                           PIC X(12).
                       07  RECEDMAKES-S-DETAIL-INOUT
                                           PIC X(1).
      *                �ݸ�����
                       07  RECEDMAKES-S-MAIN-INSURANCE.
      *                    �ݸ��ɣ�
                           09  RECEDMAKES-S-HKNID
                                           PIC X(10).
      *                    �ݸ��ֹ�
                           09  RECEDMAKES-S-INSURANCE-CLASS
                                           PIC X(03).
      *                    �ݸ����ֹ�
                           09  RECEDMAKES-S-INSURANCE-NUMBER
                                           PIC X(08).
      *                    ����
                           09  RECEDMAKES-S-MAIN-MARK
                                           PIC X(38).
      *                    �ֹ�
                           09  RECEDMAKES-S-MAIN-NUMBER
                                           PIC X(38).
      *                    �����ʬ
                           09  RECEDMAKES-S-MAIN-ASSISTANCE
                                           PIC X(01).
      *                �������
                       07  RECEDMAKES-S-PUBLIC-INSURANCE   OCCURS  4.
      *                    ����ɣ�
                           09  RECEDMAKES-S-KOHID
                                           PIC X(10).
      *                    �����ֹ�
                           09  RECEDMAKES-S-KOHNUM
                                           PIC X(03).
      *                    ��ô���ֹ�
                           09  RECEDMAKES-S-PROVIDER-NUMBER
                                           PIC X(08).
      *                    ������ֹ�
                           09  RECEDMAKES-S-RECIPIENT-NUMBER
                                           PIC X(20).
      *                �ҳ�������̵ͭ
                       07  RECEDMAKES-S-ACCIDENT-TARGET
                                           PIC X(4).
      *                �������̵ͭ
                       07  RECEDMAKES-S-INCARE-CLASS
                                           PIC X(4).
      *                ϫ�Ҿ���
                       07  RECEDMAKES-S-ACCIDENT-INSURANCE.
      *****************���¤�����
      *****************    09  RECEDMAKES-S-DISEASE-LOCATION
      *****************                    PIC X(100).
      *                    ϫ�Ҽ����ݸ���ʬ
                           09  RECEDMAKES-S-ACCIDENT
                                           PIC X(1).
      *                    ϫƯ�ݸ��ֹ�
                           09  RECEDMAKES-S-ACCIDENT-NUMBER
                                           PIC X(14).
      *                     ǯ��ڽ��ֹ�
                           09  RECEDMAKES-S-PENSIONCERTIFICATE
                                           PIC X(09).
      *                    �ҳ���ʬ
                           09  RECEDMAKES-S-ACCIDENT-CLASS
                                           PIC X(1).
       *                   �򹯴�����Ģ�ֹ�
                           09  RECEDMAKES-S-KANRITCHO-NUMBER
                                          PIC X(13).
      *                    �ǻ���
                           09  RECEDMAKES-S-RYOYO-DD
                                           PIC X(2).
     *
                       07  RECEDMAKES-S-DETAIL-WARNING
                                           PIC X(3).
                       07  RECEDMAKES-S-DETAIL-WARNING-MSG
                                           PIC X(100).
      *        ���ֻ���
               03  RECEDMAKES-PERIOD-INFO          OCCURS  10.
                   05  RECEDMAKES-START-MONTH-O
                                           PIC X(7).
                   05  RECEDMAKES-END-MONTH-O
                                           PIC X(7).
                   05  RECEDMAKES-SELECT-MODE-O
                                           PIC X(2).
      *        �ݸ����ֹ�(00000000:ľ�����᤹���ݸ��԰ʳ�)
               03  RECEDMAKES-HKNJA-OCC            OCCURS  10.
                   05  RECEDMAKES-HKNJANUM-O
                                           PIC X(8).
                   05  RECEDMAKES-HKNJANAME-O
                                           PIC X(30).
      *        ���쥻�ץȷ��
               03  RECEDMAKES-ALL-COUNT    PIC X(5).
      *        �����
               03  RECEDMAKES-ALL-NUMBER-OF-SHEETS
                                           PIC X(5).
