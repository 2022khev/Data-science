import streamlit as st

# Streamlit App Title
st.title("Simple Streamlit App")

# Text input
name = st.text_input("Enter your name:")

# Display input
if name:
    st.write(f"Hello, {name}! Welcome to the Streamlit app.")
