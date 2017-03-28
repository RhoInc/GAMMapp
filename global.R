library(shiny)
library(tidyverse)
library(ggplot2)
library(mgcv)
library(shinyBS)
library(lme4)
library(AICcmodavg)

dat <- read.table('data/toenail.txt', header=T)

safe_gamm <- safely(gamm)

