# Эксперименты и проблема множественных сравнений
# Анна Ставнийчук
# 2024-11-02
# HSE R Meet Up

# Создадим выборку по условию  --------------------------------------
{
people <- 1000
p_eat_jelly_bean <- 0.9

set.seed(1)
acne_condition <- runif(people)

# Pаметим, что тритмент экзогенен, фактически мы не закладывали никакой эффект в данные
eating <- sample(c('едят', 'не едят'), 
                 people, 
                 replace = TRUE, 
                 prob = c(p_eat_jelly_bean, 1 - p_eat_jelly_bean))

# Соберем всё в фрейм 
data <- data.frame('acne_condition' = acne_condition, 
                   'eating' = eating)
head(data)
}

# Эффект употребления желейных бобов на качество кожи  --------------
{

# Перед проведением формального статистического тестирования, проведем предварительный визуальный анализ
# Согласно боксплоту, нет существенной разницы в `acne_condition` между двумя категориями `eating`.
library(ggplot2)

ggplot(data) +
  geom_boxplot(aes(x = eating, 
                   y = acne_condition, 
                   fill = eating)) +
  scale_fill_manual(breaks = c('eating', 'not eating'),
                    values = c('grey', 'white')) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  xlab('Употребление желейных бобов') +
  ylab('Качество кожи') +
  ggtitle('Распределение качества кожи в зависимости от употребления желейных бобов')

# Формально сфоррмулируем гипотезу H_0: употребление желейных бобов не оказывает влияния на качество кожи
# Для проверки этой гипотезы проведём t-тест:

t.test(acne_condition ~ eating, data = data)

# Полученное p-значение высокое, поэтому мы не можем отвергнуть нулевую гипотезу. 
# Другими словами, мы приходим к выводу, что нет достаточных доказательств того, 
# что употребление желейных бобов вызывает акне. 
}

# Гетерогенный эффект употребления желейных бобов на качеств --------

# Сгенерируем список цветов
set.seed(1)
colors <- sample(colors(), 20)

# Рапсределим цвета по людям. Код ниже генерирует еще одну случайную величину `jelly_bean_color`.
set.seed(1)
data$jelly_bean_color <- sample(colors, people, replace = TRUE)

# Сейчас у нас есть люди, которые не ели конфеты, но у них в столбце есть цвет. Исправим это. 
library(dplyr)

data <- data %>% 
  mutate(jelly_bean_color = 
           ifelse(eating == "не едят", 
                  NA, 
                  jelly_bean_color))
head(data)

# Мы построили график распределения случаев акне для разных цветов желейных конфет.
ggplot(data %>% 
         filter(!is.na(jelly_bean_color))) +
  geom_boxplot(aes(x = jelly_bean_color, 
                   y = acne_condition, 
                   fill = jelly_bean_color)) +
  scale_fill_manual(breaks = colors, 
                    values = colors) +
  xlab('Цвет желейных бобов') +
  ylab('Состояние кожи') +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Чтобы проверить влияние определенного цвета желейных конфет на состояние акне, вы снова запускаете t-тест
test_color <- function(color){
data <- data %>%
  mutate(eating_color =
           ifelse(jelly_bean_color != color |
                    is.na(jelly_bean_color),
                  'нет', 'да'),
         color_dummy = ifelse(jelly_bean_color == color,
                              'да', 'нет'))
pval <- t.test(acne_condition ~ eating,
               subset(data, jelly_bean_color == color |
                        is.na(jelly_bean_color)))$p.value
return(pval)
}

# Проверим для нескольких цветов формально с помощью т-теста
test_color(colors[1])
test_color(colors[2])
test_color(colors[3])

# Теперь проведем t-тест для всех цветов желейных конфет и наблюдаем следующее распределение 20 p-значений.
ttest_data <- data.frame('color' = colors, 
                         'pval' = sapply(colors, test_color))
ggplot(ttest_data) +
  geom_point(aes(x = color, 
                 y = pval, 
                 color = color), 
             size = 5) +
  scale_color_manual(breaks = colors, 
                     values = colors) +
  geom_hline(aes(yintercept = 0.05), 
             linetype = 'dashed') +
  ylim(0, 1) +
  xlab('Цвет желейных бобов') +
  ylab('p-value') +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

test_color(colors[5])

# Коррекция p-value -------------------------------------------------

# Сделаем разные коррекции
ttest_data$bonferroni <- p.adjust(ttest_data$pval, method = "bonferroni")
ttest_data$holm <- p.adjust(ttest_data$pval, method = "holm")
ttest_data$bh <- p.adjust(ttest_data$pval, method = "BH")
ttest_data$by <- p.adjust(ttest_data$pval, method = "BY")
head(ttest_data)

# Переведем таблицу из широкого в длинный вид для ggplot
library(data.table)
long_ttest_data <- melt(setDT(ttest_data[,-1]), id.vars = c("pval"), variable.name = "correction_type")
head(long_ttest_data)

# Сравним результаты
ggplot(long_ttest_data, aes(x = pval, y = value, colour = correction_type)) + 
  geom_point() + 
  geom_line(linetype = 'dashed') + 
  geom_hline(yintercept = 0.05, colour = 'red', linetype = 'dashed') + 
  geom_vline(xintercept = 0.05, colour = 'red', linetype = 'dashed') +
  annotate("text", x = 0.06, y = 0.05, label = "p-value = 5%", colour = "red", hjust = 0, vjust = -0.5)

library(reshape)
wide_ttest_data <- cast(long_ttest_data,  pval ~ correction_type)

wide_ttest_data %>% arrange(pval) %>% head(10)


