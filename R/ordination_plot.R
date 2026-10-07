ordination_plot <- function(ordination){

	ord_form <- ordination$formal
	b_form <- ordination$dat_form
	ord_inform <- ordination$informal
	b_inform <- ordination$dat_inform


	# plot
	png('graphics/ordination.png', width = 8, height = 4, units = 'in', res = 450)
	mat_layout <- matrix(c(1, 1, 2, 2, 3),    # Specify layout matrix
											 nrow = 1,
											 byrow = TRUE)
	layout(mat_layout)

	ordiplot(ord_form, type = "n", xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
	ordiellipse(ord_form, groups = b_form$distance, display = "sites", kind = "ehull", conf, label = F, draw = "polygon", col = c("#3b7c70", "#ce9642"), alpha = 160)
	orditorp(ord_form, display = "species", col = "black", air = 0.1, pch = 3)
	title(main = "Formal Sites")

	ordiplot(ord_inform, type = "n", xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
	ordiellipse(ord_inform, groups = b_inform$distance, display = "sites", kind = "ehull", conf, label = F, draw = "polygon", col = c("#3b7c70", "#ce9642"), alpha = 160)
	orditorp(ord_inform, display = "species", col = "black", air = 0.1, pch = 3)
	title(main = "Informal Sites")

	plot.new()
	legend("center", legend = c("Close", "Far"), fill = c("#3b7c70", "#ce9642"), bty = "n")

	dev.off()

}
