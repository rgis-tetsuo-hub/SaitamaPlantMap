# =========================================================
# SaitamaPlantMap : deploy_to_inst() 自動同期関数
# =========================================================

#' デプロイ用ファイルを inst/app/ に自動コピー
#'
#' @description
#' /global.R, /ui.R, /server.R を inst/app/ にコピーします。
#' コピー前に、旧ファイルのバックアップを inst/app/backup/ に保存します。
#'
#' @param verbose TRUE の場合、処理ログを出力（デフォルト: TRUE）
#' @export
deploy_to_inst <- function(verbose = TRUE) {
  app_dir <- "inst/app"
  backup_dir <- file.path(app_dir, "backup")

  # ---- ディレクトリ確認 ----
  if (!dir.exists(app_dir)) {
    dir.create(app_dir, recursive = TRUE)
    if (verbose) message("[+] inst/app/ を新規作成しました。")
  }

  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir)
  }

  # ---- 同期対象 ----
  files <- c("global.R", "ui.R", "server.R")
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")

  # ---- バックアップとコピー ----
  for (f in files) {
    src <- f
    dest <- file.path(app_dir, f)

    if (file.exists(dest)) {
      backup_file <- file.path(backup_dir, paste0(f, ".", timestamp, ".bak"))
      file.copy(dest, backup_file, overwrite = TRUE)
      if (verbose) message(sprintf("[backup] %s -> %s", dest, backup_file))
    }

    if (file.exists(src)) {
      file.copy(src, dest, overwrite = TRUE)
      if (verbose) message(sprintf("[copy] %s -> %s", src, dest))
    } else {
      warning(sprintf("⚠ ファイルが存在しません: %s", src))
    }
  }

  if (verbose) {
    message("\n✅ deploy_to_inst() 完了！")
    message("inst/app/ 以下が最新化されました。")
  }
}
