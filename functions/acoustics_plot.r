acoustics_plot <- function(data, variable){
    return(
        data |>
            ggplot2::ggplot(
                ggplot2::aes(
                    x = dateTime, y = depth,
                    fill = variable,
                    colour = variable
                )
            ) +
        ggplot2::geom_point() + 
        ggplot2::scale_fill_viridis_c() +
        ggplot2::scale_colour_viridis_c() +
        ggplot2::xlab('Time') +
        ggplot2::ylab('Depth [m]') +
        ggplot2::scale_x_datetime(breaks = scales::date_breaks('12 hours'),
                           labels = function(x) format(x, format = "%m/%d %H:%M"),
                           expand = c(0, 0)) +
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = NA),
            panel.grid = ggplot2::element_blank(), 
            legend.position = 'bottom',
            legend.text =  ggplot2::element_text(size = 25),
            legend.title =  ggplot2::element_text(size = 25),
            legend.key.width = grid::unit(3,'cm'),
            legend.key.height = grid::unit(0.3,'cm'),
            axis.text.y = ggplot2::element_text(size = 25, hjust = 0),
            axis.title.y = ggplot2::element_text(size = 25, vjust = 2, hjust = 0.5),
            axis.title.x = ggplot2::element_text(size = 25, vjust = -2),
            axis.text.x = ggplot2::element_text(size = 25),
            axis.ticks.y = ggplot2::element_blank(),
            plot.margin = grid::unit(c(0.4,0.4,.4,1), "cm")
            )
    )
}
