#' Convenient cell composition plotting
#'
#' Takes a Seurat object and an annotation column name
#'
#'
#'
#'@param object A Seurat object; more input options TBA
#'@param reduction A reduction slot name in the Seurat object
#'@param reduction A metadata column name to group by
#'@export


plot_cell_types <- function(object, idents=NULL, reduction="umap"){

  df = if(!is.null(idents)) object[[idents]][,1] else Seurat::Idents(object)
  if(length(unique(df)) >16) stop("Too many categories! Max. 16 cell types supported. ")
  .pal <- pals::glasbey(length(unique(df))+5)[-c(1:5)]
  names(.pal) = as.character(levels(factor(df)))


  print(


    Seurat::DimPlot(
      object,
      reduction=reduction,
      group.by=if(!is.null(idents))idents,
      label = TRUE,
      repel=TRUE
    ) +
      {if(!is.null(.pal)) ggplot2::scale_color_manual(values=.pal)} +
      ggplot2::theme(
        axis.text=element_text(size=12, color="black", face="bold.italic")
      ) +
      Seurat::NoLegend() +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::ggtitle("") +


      df |>
      table() |>
      sort() |>
      as.data.frame() |>
      ggplot2::ggplot() +
      ggplot2::aes(x="", y=Freq/sum(Freq)*100, fill=df) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::geom_text(
        ggplot2::aes(label=round(Freq/sum(Freq)*100)),
        position=position_stack(vjust=0.5),
        size=5,
        fontface="bold.italic",
        col="white",
        hjust=0.5
      ) +
      ggplot2::scale_y_continuous(expand=c(0, 0), labels=c("0", "25 %", "50 %", "75 %", "100 %")) +
      {if(!is.null(.pal)) ggplot2::scale_fill_manual(values=.pal)} +
      ggplot2::xlab("") +
      ggplot2::ylab("")  +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.text.y = element_text(
          size=12,
          color="black",
          face="bold.italic"
        ),
        axis.ticks.x = element_blank()
      ) +
      Seurat::NoLegend() +


      df |>
      table() |>
      sort() |>
      as.data.frame() |>
      ggplot2::ggplot() +
      ggplot2::aes(x="", y=Freq/sum(Freq)*100, fill=df) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::geom_text(
        ggplot2::aes(label=df),
        position=ggpp::position_stacknudge(vjust=0.5, x=-5),
        size=4,
        fontface="bold.italic",
        col="black",
        hjust=0
      ) +
      ggplot2::scale_y_continuous(expand=c(0, 0)) +
      ggplot2::scale_fill_manual(values=rep("white", length(unique(df)))) +
      ggplot2::xlab("") +
      ggplot2::ylab("")  +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()
      ) +
      Seurat::NoLegend() +

      patchwork::plot_layout(widths = c(5,1,2))

  )



}
