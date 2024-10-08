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
      *                        COPY (CPRECEIPTDATA-PATIENT-MAKEV3REQ.INC)
      ******************************************************************
       01  XML-RECEIPTDATA-PATIENT-MAKEREQ.
           03  RECEDMAKEQ-CONTEXT          PIC S9(9)   BINARY.
           03  RECEDMAKEQ-OBJECT           PIC X(36).
           03  RECEDMAKEQ-MODE             PIC S9(9)   BINARY.
           03  RECEDMAKEQ-RECORDNAME       PIC X(128).
           03  RECEDMAKEQ-PATIENTREQ.
      *        �ꥯ�������ֹ�
               05  RECEDMAKEQ-REQUEST-NUMBER
                                           PIC X(2).
      *        �����UID
               05  RECEDMAKEQ-KARTE-UID    PIC X(36).
      *        ���륫UID
               05  RECEDMAKEQ-ORCA-UID     PIC X(36).
      *        �»���
               05  RECEDMAKEQ-PERFORM-DATE PIC X(10).
      *        ����ǯ��
               05  RECEDMAKEQ-PERFORM-MONTH
                                           PIC X(7).
      *        ����ǯ����
               05  RECEDMAKEQ-AC-DATE      PIC X(10).
      *        �����⡼��(Check:������ Ckeck�ʳ�:�����)
               05  RECEDMAKEQ-CREATE-MODE  PIC X(6).
      *        �����
               05  RECEDMAKEQ-SUBMISSION-MODE
                                           PIC X(2).
      *        �쥻�ťǡ��������å�(Yes:�����å����� Yes�ʳ�:�����å����ʤ�)
               05  RECEDMAKEQ-CHECK-MODE   PIC X(3).
      *        �ݸ����ֹ�(00000000:ľ�����᤹���ݸ��԰ʳ�)
               05  RECEDMAKEQ-HKNJANUM     PIC X(8).
      *        ���ֻ���
               05  RECEDMAKEQ-START-MONTH  PIC X(7).
               05  RECEDMAKEQ-END-MONTH    PIC X(7).
      *        �����оݴ��԰���
               05  RECEDMAKEQ-PATIENT-INFORMATION OCCURS  100.
      *            �����оݡݴ����ֹ�
                   07 RECEDMAKEQ-PATIENT-ID       PIC X(20).
      *            �����оݡݿ���ǯ��
                   07 RECEDMAKEQ-PATIENT-MONTH    PIC X(7).
      *            �����оݡ�������ʬ
                   07 RECEDMAKEQ-PATIENT-INOUT    PIC X(2).
      *            �����оݡݥ쥻�ץȾ���
                   07 RECEDMAKEQ-DETAIL-INFORMATION-G.
                     08 RECEDMAKEQ-DETAIL-INFORMATION   OCCURS  5.
      *                ���̥�����
                       09  RECEDMAKEQ-DETAIL-CODE
                                           PIC X(04).
      *                ����
                       09  RECEDMAKEQ-DETAIL-TYPE
                                           PIC X(12).
                       09  RECEDMAKEQ-DETAIL-INOUT
                                           PIC X(1).
      *                �ݸ�����
                       09  RECEDMAKEQ-MAIN-INSURANCE.
      *                    �ݸ��ɣ�
                           11  RECEDMAKEQ-HKNID
                                           PIC X(10).
      *                    �ݸ��ֹ�
                           11  RECEDMAKEQ-INSURANCE-CLASS
                                           PIC X(03).
                           11  RECEDMAKEQ-MAIN-INSURANCE-1.
      *                        �ݸ����ֹ�
                               13  RECEDMAKEQ-INSURANCE-NUMBER
                                           PIC X(08).
      *                        ����
                               13  RECEDMAKEQ-MAIN-MARK
                                           PIC X(38).
      *                        �ֹ�
                               13  RECEDMAKEQ-MAIN-NUMBER
                                           PIC X(38).
      *                        �����ʬ
                               13  RECEDMAKEQ-MAIN-ASSISTANCE
                                           PIC X(01).
      *                �������
                       09  RECEDMAKEQ-PUBLIC-INSURANCE.
                           11  RECEDMAKEQ-PUBLIC-INSURANCE-O  OCCURS  4.
      *                        ����ɣ�
                               13  RECEDMAKEQ-KOHID
                                               PIC X(10).
      *                        �����ֹ�
                               13  RECEDMAKEQ-KOHNUM
                                           PIC X(03).
      *                        ��ô���ֹ�
                               13  RECEDMAKEQ-PROVIDER-NUMBER
                                               PIC X(08).
      *                        ������ֹ�
                               13  RECEDMAKEQ-RECIPIENT-NUMBER
                                               PIC X(20).
      *                �ҳ�������̵ͭ
                       09  RECEDMAKEQ-ACCIDENT-TARGET
                                           PIC X(4).
      *                �������̵ͭ
                       09  RECEDMAKEQ-INCARE-CLASS
                                           PIC X(4).
      *                ϫ�Ҿ���
                       09  RECEDMAKEQ-ACCIDENT-INSURANCE.
      *****************    ���¤�����
      *****************    11  RECEDMAKEQ-DISEASE-LOCATION
      *****************                    PIC X(100).
                           11  RECEDMAKEQ-ACCIDENT-INSURANCE-1.
      *                        ϫ�Ҽ����ݸ���ʬ
                               13  RECEDMAKEQ-ACCIDENT
                                           PIC X(01).
      *                        ϫƯ�ݸ��ֹ�
                               13  RECEDMAKEQ-ACCIDENT-NUMBER
                                           PIC X(14).
      *                        ǯ��ڽ��ֹ�
                               13  RECEDMAKEQ-PENSIONCERTIFICATE
                                           PIC X(09).
      *                        �ҳ���ʬ
                               13  RECEDMAKEQ-ACCIDENT-CLASS
                                           PIC X(1).
                           11  RECEDMAKEQ-ACCIDENT-INSURANCE-2.
      *                        �򹯴�����Ģ�ֹ�
                               13  RECEDMAKEQ-KANRITCHO-NUMBER
                                          PIC X(13).
      *                        ����ǯ����
                               13  RECEDMAKEQ-RYOYO-DD
                                           PIC X(2).
      *                �»ܳ�ǧ�ؼ��ʷٹ��
                       09  RECEDMAKEQ-RECEIPT-PERFORM-FLAG
                                           PIC X(4).
      *
