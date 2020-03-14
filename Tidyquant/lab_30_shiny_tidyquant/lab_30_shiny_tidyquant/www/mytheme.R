library(fresh)
create_theme(
    theme = "paper",
    bs_vars_navbar(
        default_bg = "#2C3E50",
        height = "60px",
        default_color = "#FFFFFF",
        default_link_color = "#FFFFFF",
        default_link_active_color = '#18BC9C',
        default_link_hover_color = '#18BC9C',
        inverse_link_color = '#FFFFFF',
        inverse_link_active_color = '#18BC9C',
        inverse_link_hover_color = '#18BC9C'
    ),
    bs_vars_color(
        gray_base = "#354e5c",
        brand_primary = "#2C3E50",
        brand_success = "#18BC9C",
        brand_info = '#3498db',
        brand_warning = "#FF7F00",
        brand_danger = "#E31A1C"
    ),
    bs_vars_state(
        success_text = "#18BC9C",
        success_bg = "#FFF",
        success_border = "#18BC9C",
        info_text = '#3498db',
        info_bg = '#3498db',
        info_border = '#3498db',
        warning_text = "#FF7F00",
        warning_bg = "#FF7F00",
        warning_border = "#FF7F00",
        danger_text = "#E31A1C",
        danger_bg = "#E31A1C",
        danger_border = "#E31A1C"
    ),
    bs_vars_wells(
        bg = "#FFF",
        border = "#3f2d54"
    ),
    bs_vars_button(
        success_color = "#FFF",
        success_bg = "#18BC9C",
        success_border = "#18BC9C",
        info_color = '#3498db',
        info_bg = '#3498db',
        info_border = '#3498db',
        warning_color = "#FF7F00",
        warning_bg = "#FF7F00",
        warning_border = "#FF7F00",
        danger_color = "#E31A1C",
        danger_bg = "#E31A1C",
        danger_border = "#E31A1C" 
    ),
    output_file = "www/mytheme.css"
)

