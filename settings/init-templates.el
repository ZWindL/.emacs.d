;;; init-templates.el --- Snippets -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'skeleton)

(define-skeleton insert-date-time
  "Insert current date time with 2 date formats available."
  ""
  > (let ((full (format-time-string "%F %T"))
          (local (format-time-string "%c")))
      (completing-read "datetime: " `(,full ,local))))

(define-skeleton insert-mail-signature
  "Insert mail signature in org-mode format."
  ""
  > "Regards,\n\n"
  > "#+begin_signature\n"
  > "-- *" (skeleton-read "Your signature: ") "*\n"
  > "#+end_signature\n")

(define-skeleton insert-zf-tech-doc
  "Create general template of internal documents."
  ""
  "#+TITLE:" (skeleton-read "Title: ") "\n"
  "* 本方案解决什么问题\n"
  "** 业务背景说明\n"
  _"\n"
  "** 整体流程优化\n"
  "** 如何量化（指标）\n"
  "** 小组间协同\n"
  "** 产品原型链接、产品调研文档\n"
  "* 技术方案概述\n"
  (if (y-or-n-p "模块上下游关系? ")
      "* 模块上下游关系（流程图）\n")
  "* 模块内流程图\n"
  (if (y-or-n-p "重点模块拆解? ")
      "* 重点模块拆解
      如果是数据分析功能，需含有数据逻辑推导过程\n")
  (if (y-or-n-p "数据库设计? ")
      "* 数据库设计\n
    新数据表需给出详细schema设计
    已有数据表优化需给出变更信息，以及变更成本分析
    数据规模预估\n")
  "* 对用户增长的影响
    对 SEO 的影响\n对现有营销活动的影响，需要业务负责人&PM确认；\n"
  "* 部署方案\n** 机器:\n** 调度:\n"
  "* 评估方案 & 测试要点\n"
  (if (y-or-n-p "数据监控? ")
      "* 数据监控&统计方案\n")
  (if (y-or-n-p "业务方案? ")
      "* 业务目标
    对用户指标的影响:\n比如预计提升留存1%
    模块平均频次上升10%\n")
  "* 人力排期\n"
  "* 隐患\n** 技术:\n** 人力:\n** 外部不可控因素\n"
  (if (y-or-n-p "是否有参考? ")
      "* 相关技术方案调研参考
    技术选型的依据；
    内部研发经验；"))

(provide 'init-templates)
;;; init-templates.el ends here
