# ==============================================================================
# CALCULATIONS - Sources all sheets and computes all metrics
# ==============================================================================
# RUN FROM: Project root directory
# All dates are now determined dynamically from the database
# ==============================================================================

# Load common helpers
source("helpers.R")

# Load config (for COVID/Election reference labels only)
source("config.R")

# Load all sheets
source("lfs.R")
source("vacancies.R")
source("payroll.R")
source("wages_nominal.R")
source("wages_cpi.R")
source("days_lost.R")
source("redundancy.R")
source("sector_payroll.R")
source("hr1.R")
source("inactivity_reasons.R")
source("summary.R")



# ==============================================================================
# CALCULATE ALL METRICS (no manual_month - all dynamic)
# ==============================================================================

# LFS metrics (employment, unemployment, inactivity)
lfs <- calculate_lfs()

# Vacancies
vac <- calculate_vacancies()

# Payrolled employees
payroll <- calculate_payroll()

# Nominal wages (total + regular)
wages_nom <- calculate_wages_nominal()

# CPI-adjusted wages (total + regular)
wages_cpi <- calculate_wages_cpi()

# Working days lost
days_lost <- calculate_days_lost()

# Redundancy (LFS)
redund <- calculate_redundancy()

# Sector payroll (hospitality, retail, health)
sectors <- calculate_sector_payroll()

# HR1 redundancy notifications
hr1 <- calculate_hr1()

# Inactivity by reason
inact_reasons <- calculate_inactivity_reasons()

# ==============================================================================
# ANCHOR DATE FOR LABELS (derived from LFS data)
# ==============================================================================

anchor_m <- lfs$emp16$end

# ==============================================================================
# CONVENIENCE VARIABLES FOR DASHBOARD
# ==============================================================================

# Employment
emp16_cur <- lfs$emp16$cur
emp16_dq <- lfs$emp16$dq
emp16_dy <- lfs$emp16$dy
emp16_dc <- lfs$emp16$dc
emp16_de <- lfs$emp16$de

emp_rt_cur <- lfs$emp_rt$cur
emp_rt_dq <- lfs$emp_rt$dq
emp_rt_dy <- lfs$emp_rt$dy
emp_rt_dc <- lfs$emp_rt$dc
emp_rt_de <- lfs$emp_rt$de

# Unemployment
unemp16_cur <- lfs$unemp16$cur
unemp16_dq <- lfs$unemp16$dq
unemp16_dy <- lfs$unemp16$dy
unemp16_dc <- lfs$unemp16$dc
unemp16_de <- lfs$unemp16$de

unemp_rt_cur <- lfs$unemp_rt$cur
unemp_rt_dq <- lfs$unemp_rt$dq
unemp_rt_dy <- lfs$unemp_rt$dy
unemp_rt_dc <- lfs$unemp_rt$dc
unemp_rt_de <- lfs$unemp_rt$de

# Inactivity
inact_cur <- lfs$inact$cur
inact_dq <- lfs$inact$dq
inact_dy <- lfs$inact$dy
inact_dc <- lfs$inact$dc
inact_de <- lfs$inact$de

inact_rt_cur <- lfs$inact_rt$cur
inact_rt_dq <- lfs$inact_rt$dq
inact_rt_dy <- lfs$inact_rt$dy
inact_rt_dc <- lfs$inact_rt$dc
inact_rt_de <- lfs$inact_rt$de

# 50-64 Inactivity
inact5064_cur <- lfs$inact5064$cur
inact5064_dq <- lfs$inact5064$dq
inact5064_dy <- lfs$inact5064$dy
inact5064_dc <- lfs$inact5064$dc
inact5064_de <- lfs$inact5064$de

inact5064_rt_cur <- lfs$inact5064_rt$cur
inact5064_rt_dq <- lfs$inact5064_rt$dq
inact5064_rt_dy <- lfs$inact5064_rt$dy
inact5064_rt_dc <- lfs$inact5064_rt$dc
inact5064_rt_de <- lfs$inact5064_rt$de

# Vacancies
vac_cur <- vac$cur
vac_dq <- vac$dq
vac_dy <- vac$dy
vac_dc <- vac$dc
vac_de <- vac$de

# Payroll (dashboard - 3 month avg)
payroll_cur <- payroll$cur
payroll_dq <- payroll$dq
payroll_dy <- payroll$dy
payroll_dc <- payroll$dc
payroll_de <- payroll$de



# Payroll flash (narrative - latest single month)
payroll_flash_cur <- payroll$flash_cur
payroll_flash_dy <- payroll$flash_dy
payroll_flash_de <- payroll$flash_de
payroll_flash_dm <- payroll$flash_dm

# Nominal wages - Total
latest_wages <- wages_nom$total$cur
wages_change_q <- wages_nom$total$dq
wages_change_y <- wages_nom$total$dy
wages_change_covid <- wages_nom$total$dc
wages_change_election <- wages_nom$total$de
wages_total_public <- wages_nom$total$public
wages_total_private <- wages_nom$total$private
wages_total_qchange <- wages_nom$total$qchange

# Nominal wages - Regular
latest_regular_cash <- wages_nom$regular$cur
wages_reg_change_q <- wages_nom$regular$dq
wages_reg_change_y <- wages_nom$regular$dy
wages_reg_change_covid <- wages_nom$regular$dc
wages_reg_change_election <- wages_nom$regular$de
wages_reg_public <- wages_nom$regular$public
wages_reg_private <- wages_nom$regular$private
wages_reg_qchange <- wages_nom$regular$qchange

# CPI wages - Total
latest_wages_cpi <- wages_cpi$total$cur
wages_cpi_change_q <- wages_cpi$total$dq
wages_cpi_change_y <- wages_cpi$total$dy
wages_cpi_change_covid <- wages_cpi$total$dc
wages_cpi_change_election <- wages_cpi$total$de
wages_cpi_total_vs_dec2007 <- wages_cpi$total$pct_vs_dec2007
wages_cpi_total_vs_pandemic <- wages_cpi$total$pct_vs_pandemic

# CPI wages - Regular
latest_regular_cpi <- wages_cpi$regular$cur
wages_reg_cpi_change_q <- wages_cpi$regular$dq
wages_reg_cpi_change_y <- wages_cpi$regular$dy
wages_reg_cpi_change_covid <- wages_cpi$regular$dc
wages_reg_cpi_change_election <- wages_cpi$regular$de
wages_cpi_reg_vs_dec2007 <- wages_cpi$regular$pct_vs_dec2007
wages_cpi_reg_vs_pandemic <- wages_cpi$regular$pct_vs_pandemic

# Days lost
days_lost_cur <- days_lost$cur
days_lost_label <- days_lost$label

# Redundancy
redund_cur <- redund$cur
redund_dq <- redund$dq
redund_dy <- redund$dy
redund_dc <- redund$dc
redund_de <- redund$de

# Sectors - Hospitality
hosp_cur <- sectors$hospitality$cur
hosp_dm <- sectors$hospitality$dm
hosp_dy <- sectors$hospitality$dy
hosp_dc <- sectors$hospitality$dc
hosp_de <- sectors$hospitality$de

# Sectors - Retail
retail_cur <- sectors$retail$cur
retail_dm <- sectors$retail$dm
retail_dy <- sectors$retail$dy
retail_dc <- sectors$retail$dc
retail_de <- sectors$retail$de

# Sectors - Health
health_cur <- sectors$health$cur
health_dm <- sectors$health$dm
health_dy <- sectors$health$dy
health_dc <- sectors$health$dc
health_de <- sectors$health$de

# HR1
hr1_cur <- hr1$cur
hr1_dm <- hr1$dm
hr1_dy <- hr1$dy
hr1_dc <- hr1$dc
hr1_de <- hr1$de

# Inactivity driver text (for narrative)
inact_driver_text <- generate_inactivity_driver_text(inact_reasons)

# ==============================================================================
# NARRATIVE LABELS
# ==============================================================================

lfs_period_label <- lfs_label_narrative(lfs$emp16$end)
payroll_month_label <- format(payroll$anchor, "%B %Y")
payroll_flash_label <- format(payroll$flash_anchor, "%B %Y")
sector_month_label <- format(sectors$hospitality$anchor, "%B %Y")
hr1_month_label <- format(hr1$anchor, "%B %Y")

# ==============================================================================
# BRIEFING RELEASE DATE LABEL (for UI display)
# ==============================================================================

# The "release" label is based on the LFS period end date + 2 months
# e.g., if LFS data is for Oct 2025 (Aug-Oct), the briefing is "December 2025"
briefing_release_label <- if (!is.na(anchor_m)) {
  format(anchor_m %m+% months(2), "%B %Y")
} else {
  format(Sys.Date(), "%B %Y")
}
