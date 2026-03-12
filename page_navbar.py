"""Reusable navbar component for Streamlit pages."""

import streamlit as st

UNIR_LOGO_URL = "https://www.unir.net/wp-content/themes/unir-2023/assets/images/logo-unir.svg"


def page_navbar(title: str, logo_url: str = UNIR_LOGO_URL) -> None:
    """Render the main project navbar.

    Args:
        title: Main navbar heading.
        logo_url: URL for the UNIR logo image.
    """

    st.markdown(
        f"""
        <style>
            .ap-navbar {{
                background-color: #0057B8;
                color: #FFFFFF;
                border-radius: 12px;
                padding: 14px 18px;
                display: flex;
                align-items: center;
                gap: 14px;
            }}
            .ap-navbar * {{
                color: #FFFFFF !important;
            }}
            .ap-logo-wrap {{
                width: 52px;
                height: 52px;
                flex: 0 0 52px;
                position: relative;
            }}
            .ap-logo-img {{
                width: 52px;
                height: 52px;
                object-fit: contain;
                display: block;
            }}
            .ap-logo-fallback {{
                width: 52px;
                height: 52px;
                border-radius: 6px;
                display: none;
                align-items: center;
                justify-content: center;
                background: #0057B8;
                border: 1px solid rgba(255, 255, 255, 0.25);
                color: #FFFFFF !important;
                font-weight: 800;
                font-size: 30px;
                font-family: Arial, sans-serif;
            }}
            .ap-text h1 {{
                margin: 0;
                font-size: 1.15rem;
                line-height: 1.2;
                font-weight: 700;
            }}
            .ap-text p {{
                margin: 0.2rem 0 0;
                font-size: 0.86rem;
                opacity: 0.98;
            }}
        </style>

        <div class="ap-navbar">
            <div class="ap-logo-wrap">
                <img
                    class="ap-logo-img"
                    src="{logo_url}"
                    alt="Logo UNIR"
                    onerror="this.style.display='none'; this.nextElementSibling.style.display='flex';"
                />
                <div class="ap-logo-fallback" aria-label="UNIR fallback logo">U</div>
            </div>
            <div class="ap-text">
                <h1>{title}</h1>
                <p>Atlas de la Polarización 2025 · More in Common · UNIR</p>
            </div>
        </div>
        """,
        unsafe_allow_html=True,
    )
