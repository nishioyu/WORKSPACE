--                                    --
-- ���̼�Ǽ�ޥ��������ץ쥳���ɺ��   --
--                                    --
--   ������                         --
--   ��ɼ�ֹ�9000000--9099999���о�   --
--   ������ʬ = '1'                   --
--   ��ɼ������ʬ = '3'               --
--                                    --
-- Create Date : 2005/07/14           --
--                                    --


-- ���ץ쥳���ɤκ��            --

DELETE FROM TBL_SYUDAY WHERE NYUGAIKBN = '1' AND DENPNUM >= 9000000 AND
                             DENPNUM <= 9099999 AND CREATEKBN = '3';

